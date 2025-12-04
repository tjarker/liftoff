package liftoff.simulation.verilator


import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import liftoff.misc.SharedObject

import liftoff.misc.WorkingDirectory
import liftoff.simulation.PortHandle
import coursier.core.Repository.Complete.Input.Ver

object VerilatorModelHarness {

  def createContextFunName(m: String) = s"${m}_create_context"
  def deleteContextFunName(m: String) = s"${m}_delete_context"
  def evalFunName(m: String) = s"${m}_eval"
  def tickFunName(m: String) = s"${m}_tick"
  def setFunName(m: String) = s"${m}_set"
  def getFunName(m: String) = s"${m}_get"
  def setWideFunName(m: String) = s"${m}_set_wide"
  def getWideFunName(m: String) = s"${m}_get_wide"

  def imports(m: String) =
    s"""|#include <verilated.h>
        |#include <verilated_fst_c.h>
        |#include <stdint.h>
        |#include "V$m.h"
        |#include "V${m}___024root.h"
        |""".stripMargin

  def contextStruct(m: String) =
    s"""|struct ${m}_context_t {
        |  uint64_t time;
        |  VerilatedContext* context;
        |  V$m* model;
        |  VerilatedFstC* trace;
        |};
        |""".stripMargin

  def createContext(m: String) =
    s"""|${m}_context_t* ${createContextFunName(m)}(const char* fstFile, const char* time_unit, char** argv, int argc) {
        |  ${m}_context_t* ctx = new ${m}_context_t;
        |  ctx->time = 0;
        |  ctx->context = new VerilatedContext;
        |  ctx->context->commandArgs(argc, argv);
        |  ctx->context->traceEverOn(true);
        |
        |  ctx->model = new V$m(ctx->context, "Circuit");
        |
        |  ctx->trace = new VerilatedFstC;
        |  ctx->model->trace(ctx->trace, 99);
        |  ctx->trace->set_time_unit(time_unit);
        |  ctx->trace->set_time_resolution(time_unit);
        |  ctx->trace->open(fstFile);
        |
        |  return ctx;
        |}
        |""".stripMargin

  def deleteContext(m: String) =
    s"""|void ${deleteContextFunName(m)}(${m}_context_t* ctx) {
        |  ctx->model->final();
        |  ctx->trace->dump(ctx->time);
        |  ctx->trace->flush();
        |  ctx->trace->close();
        |
        |  delete ctx->trace;
        |  delete ctx->model;
        |  delete ctx->context;
        |  delete ctx;
        |}
        |""".stripMargin

  def eval(m: String) =
    s"""|void ${evalFunName(m)}(${m}_context_t* ctx) {
        |  ctx->model->eval();
        |}
        |""".stripMargin

  def tick(m: String) =
    s"""|void ${tickFunName(m)}(${m}_context_t* ctx, uint64_t delta) {
        |  ctx->model->eval();
        |  ctx->trace->dump(ctx->time);
        |  ctx->time += delta;
        |}
        |""".stripMargin

  def set(m: String, syms: Seq[VerilatorPortHandle]) = {
    val cases = syms
      .filter(_.width <= 64)
      .collect {
        case i: VerilatorInputPortHandle =>
          s"""|case ${i.id}: // ${i.toString()}
              |  ctx->model->${i.name} = value;
              |  break;""".stripMargin
      }
      .mkString("\n")

    s"""|void ${setFunName(m)}(${m}_context_t* ctx, uint64_t id, uint64_t value) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def setWide(m: String, syms: Seq[VerilatorPortHandle]) = {
    val cases = syms
      .filter(_.width > 64)
      .map(s => s -> (s.width / 32d).ceil.toInt)
      .collect { case (i @ VerilatorInputPortHandle(model, name, id, width), words) =>
        s"""|case $id: // ${i.toString()}
            |  for (int i = 0; i < $words; i++) 
            |    ctx->model->${name}.data()[i] = value[i];
            |  break;
            |""".stripMargin
      }
      .mkString("\n")

    s"""|void ${setWideFunName(m)}(${m}_context_t* ctx, uint64_t id, uint32_t value[]) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def get(m: String, syms: Seq[VerilatorPortHandle]) = {
    val cases = syms
      .filter(_.width <= 64)
      .collect { case s: VerilatorPortHandle =>
        s"""|case ${s.id}:
              |  return ctx->model->${s.name};
              |""".stripMargin
      }
      .mkString("\n")

    s"""|uint64_t ${getFunName(m)}(${m}_context_t* ctx, uint64_t id) {
        |  printf("Getting value for id %lu\\n", id);
        |  switch (id) {
        |${cases.indent(4)}
        |    default:
        |      return 0; // or handle error
        |  }
        |}
        |""".stripMargin
  }

  def getWide(m: String, syms: Seq[VerilatorPortHandle]) = {
    val cases = syms
      .filter(_.width > 64)
      .map(s => s -> (s.width / 32d).ceil.toInt)
      .collect { case (i @ VerilatorPortHandle(_, name, id), words) =>
        s"""|case $id: // ${i.toString()}
            |  for (int i = 0; i < $words; i++) 
            |    value[i] = ctx->model->${name}.data()[i];
            |  break;
            |""".stripMargin
      }
      .mkString("\n")

    s"""|void ${getWideFunName(m)}(${m}_context_t* ctx, uint64_t id, uint32_t value[]) {
        |  switch (id) {
        |${cases.indent(4)}
        |  }
        |}
        |""".stripMargin
  }

  def harness(m: String, syms: Seq[VerilatorPortHandle]): String =
    s"""|${imports(m)}
        |
        |double sc_time_stamp() { return 0; }
        |
        |${contextStruct(m)}
        |extern "C" {
        |${createContext(m)}
        |${deleteContext(m)}
        |${eval(m)}
        |${tick(m)}
        |${set(m, syms).indent(2)}
        |${setWide(m, syms).indent(2)}
        |${get(m, syms).indent(2)}
        |${getWide(m, syms).indent(2)}
        |  void quack() {
        |    printf("Quack!\\n");
        |  }
        |}
        |""".stripMargin


  def writeHarness(dir: WorkingDirectory, m: String, syms: Seq[VerilatorPortHandle]) = {
    dir.addFile(s"${m}_harness.cpp", harness(m, syms))
  }

}

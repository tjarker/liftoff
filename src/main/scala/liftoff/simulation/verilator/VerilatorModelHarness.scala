package liftoff.simulation.verilator


import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import liftoff.misc.SharedObject

import liftoff.misc.WorkingDirectory
import liftoff.simulation.PortHandle

object VerilatorModelHarness {

  def createContextFunName(m: String) = s"${m}_create_context"
  def deleteContextFunName(m: String) = s"${m}_delete_context"
  def evalFunName(m: String) = s"${m}_eval"
  def tickFunName(m: String) = s"${m}_tick"
  def quackFunName(m: String) = s"${m}_quack"
  def getPointerFunName(m: String) = s"${m}_get_pointer"

  def imports(m: String) =
    s"""|#include <verilated.h>
        |#include <verilated_fst_c.h>
        |#include <stdint.h>
        |#include "V$m.h"
        |#include "V${m}___024root.h"
        |""".stripMargin

  def contextStruct(moduleName: String, functionPrefix: String) =
    s"""|struct ${functionPrefix}_context_t {
        |  uint64_t time;
        |  VerilatedContext* context;
        |  V${moduleName}* model;
        |  VerilatedFstC* trace;
        |};
        |""".stripMargin

  def createContext(m: String, p: String) =
    s"""|${p}_context_t* ${createContextFunName(p)}(const char* fstFile, const char* time_unit, char** argv, int argc) {
        |  ${p}_context_t* ctx = new ${p}_context_t;
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

  def getPointer(moduleName: String, syms: Seq[VerilatorPortDescriptor]): String = {
    val cases = syms
      .map(s => s -> (s.width / 32d).ceil.toInt)
      .collect { case (i @ VerilatorPortDescriptor(name, id, width), words) =>
        s"""|case $id: // ${i.toString()}
            |  return (void*)&ctx->model->${name};
            |""".stripMargin
      }
      .mkString("\n")

    s"""|void* ${moduleName}_get_pointer(${moduleName}_context_t* ctx, uint64_t id) {
        |  switch (id) {
        |${cases.indent(4)}
        |    default:
        |      return nullptr; // or handle error
        |  }
        |}
        |""".stripMargin
  }

  def harness(moduleName: String, functionPrefix: String, syms: Seq[VerilatorPortDescriptor]): String =
    s"""|${imports(moduleName)}
        |
        |double sc_time_stamp() { return 0; }
        |
        |${contextStruct(moduleName, functionPrefix)}
        |extern "C" {
        |${createContext(moduleName, functionPrefix).indent(2)}
        |${deleteContext(functionPrefix).indent(2)}
        |${eval(functionPrefix).indent(2)}
        |${tick(functionPrefix).indent(2)}
        |${getPointer(functionPrefix, syms).indent(2)}
        |  void ${quackFunName(functionPrefix)}() {
        |    printf("Quack $functionPrefix!\\n");
        |  }
        |}
        |""".stripMargin

  def writeHarness(dir: WorkingDirectory, moduleName: String, functionPrefix: String, syms: Seq[VerilatorPortDescriptor]) = {
    dir.addFile(s"${functionPrefix}_harness.cpp", harness(moduleName, functionPrefix, syms))
  }

}

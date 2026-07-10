package liftoff.native

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import liftoff._

import java.lang.foreign._
import java.lang.foreign.ValueLayout._
import java.lang.invoke.MethodType
import java.lang.invoke.MethodHandles
import liftoff.misc.Library

class DpiToScala extends AnyWordSpec with Matchers {

  object ScalaCallbacks {
    def eval(a: Int, b: Int, sum: MemorySegment): Unit = {
      sum.reinterpret(4).set(JAVA_INT, 0, a + b)     // write the output port
      println(s"Scala: a=$a b=$b -> sum=${a + b}")
    }
  }

  "DPI to Scala" should {
    "be able to call a Scala function from Verilog" in {
      
      val dir = "build/dpi_to_scala".toDir
      dir.createIfNotExists()
      dir.clean()

      val verilogFile = s"""
        |module top(
        |  input logic clk,
        |  input logic [31:0] a,
        |  input logic [31:0] b,
        |  output logic [31:0] sum
        |);
        |  import "DPI-C" function void call_scala(
        |  input int a,
        |  input int b,
        |  output int sum);
        |  always @(posedge clk) begin
        |    call_scala(a, b, sum);
        |  end
        |endmodule
      """.stripMargin

      val vlogFile = dir.addFile("top.sv", verilogFile)

      val cFunction = s"""
        |#include <stdio.h>
        |
        |#include <stdint.h>
        |
        |static void (*g_cb)(int, int, int*) = nullptr;
        |
        |extern "C" void register_scala_cb(void* fp) {
        |  g_cb = (void (*)(int, int, int*))fp;
        |}
        |
        |extern "C" void call_scala(int a, int b, int* sum) {   // the DPI import
        |  printf("Before C: a=%d b=%d -> sum=%d\\n", a, b, *sum);
        |  fflush(stdout);
        |  if (g_cb) g_cb(a, b, sum);
        |  printf("After C: a=%d b=%d -> sum=%d\\n", a, b, *sum);
        |  fflush(stdout);
        |}
      """.stripMargin

      val cFile = dir.addFile("call_scala.cpp", cFunction)

      val module = VerilogModel("top", Seq(vlogFile, cFile), dir, Seq(), Seq("verilator/call_scala.o"))

      val arena  = Arena.ofShared()                 // must outlive the whole simulation
      val linker = Linker.nativeLinker()
      val lookup = SymbolLookup.libraryLookup("build/dpi_to_scala/libtop_0.dylib", arena)
      val lib = new Library(lookup, linker)

      // MethodHandle to ScalaCallbacks.eval, typed (int,int,MemorySegment)void
      val mt     = MethodType.methodType(Void.TYPE, classOf[Int], classOf[Int], classOf[MemorySegment])
      val target = MethodHandles.lookup()
        .findVirtual(ScalaCallbacks.getClass, "eval", mt)
        .bindTo(ScalaCallbacks)                      // bind the object singleton

      // native function pointer that calls back into the JVM
      val stub: MemorySegment = linker.upcallStub(
        target,
        FunctionDescriptor.ofVoid(JAVA_INT, JAVA_INT, ADDRESS),
        arena
      )

      // hand it to the C side (register_scala_cb lives in the same loaded lib)
      val register = lib.functionHandle("register_scala_cb", FunctionDescriptor.ofVoid(ADDRESS))
      register.invokeExact(stub): Unit

      module.simulate(dir) { top =>        

        top.addClockDomain("clk", 1.ns)(
          top("a"), top("b"), top("sum")
        )

        for (a <- 0 until 10) {
          for (b <- 0 until 10) {
            top("a").poke(a)
            top("b").poke(b)
            top("clk").step(1)
            val sum = top("sum").peek()
            sum should be (a + b)
          }
        }



      }
    }
  }
  
}

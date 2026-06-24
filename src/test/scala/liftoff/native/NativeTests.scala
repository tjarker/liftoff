package liftoff.native

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import liftoff._
import liftoff.misc.SharedObject
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout
import java.lang.foreign._

class NativeTests extends AnyWordSpec with Matchers {

  "Native" should {
    "be available" in {


      val cFile = s"""
        |#include <stdio.h>
        |#include <stdlib.h>
        |extern "C" {
        |  void* quack();
        |}
        |void* quack() {
        |  printf("Quack!\\n");
        |  unsigned int* p = (unsigned int*)malloc(4 * sizeof(unsigned int));
        |  p[0] = 0x04030201;
        |  p[1] = 0x08070605;
        |  p[2] = 0x0c0b0a09;
        |  p[3] = 0x100f0e0d;
        |  return p;
        |}
        |""".stripMargin

      val buildDir = "build/native_test".toDir
      val quackFile = buildDir.addFile("quack.c", cFile)

      val libRecipe = SharedObject.createRecipe(
        libname = "libquack",
        dir = buildDir,
        sources = Seq(quackFile),
        options = Seq()
      )

      val lib = libRecipe.invoke().load()

      val quackHandle = lib.functionHandle(
        "quack",
        java.lang.foreign.FunctionDescriptor.of(
          java.lang.foreign.ValueLayout.ADDRESS
        )
      )

      val resultPtr: MemorySegment = quackHandle.invokeExact()
      val ptr = resultPtr.reinterpret(4 * 4)


      // print bytes in hex
      for (i <- 0 until 16) {
        val word = ptr.get(ValueLayout.JAVA_BYTE, i)
        print(f"$word%02x ")
      }


      // interpret this as a BigInt
      // BigInt takes a big endian byte array, so we need to reverse the order of the words
      // create a new memory segment and copy the bytes in reverse order
      val arena = Arena.ofShared()
      val reversedBytes = arena.allocate(4 * 4)
      for (i <- 0 until 16) {
        val word = ptr.get(ValueLayout.JAVA_BYTE, i)
        reversedBytes.set(ValueLayout.JAVA_BYTE, (15 - i), word)
      }

      val resultBigInt = BigInt(reversedBytes.toArray(ValueLayout.JAVA_BYTE))
      println(s"Result: ${resultBigInt.toString(16)}")


      val newBigInt = BigInt("0102030405060708090a0b0c0d0e0f10", 16)

      val bytes = newBigInt.toByteArray

      for (byte <- bytes) {
        print(f"$byte%02x ")
        ptr.set(ValueLayout.JAVA_BYTE, 15 - bytes.indexOf(byte), byte)
      }

      println()

      for (i <- 0 until 16) {
        val word = ptr.get(ValueLayout.JAVA_BYTE, i)
        print(f"$word%02x ")
      }


    }
  }

}
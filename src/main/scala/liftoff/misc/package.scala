package liftoff

import java.io.File

package object misc {

  implicit class PathToFileOps(val path: String) extends AnyVal {
    def toFile: File = new File(path)
    def toDir: WorkingDirectory = WorkingDirectory(path)
  }

  implicit class BigIntOps(val x: BigInt) {
    def toWordArray: Array[Int] = {
      (0 until (x.bitLength + 31) / 32)
        .map { i =>
          (x >> (i * 32)).toInt
        }.toArray
    }
  }

  implicit class WordArrayOps(val arr: Array[Int]) {
    def toBigInt: BigInt = {
      arr.foldRight(BigInt(0)) { (word, acc) =>
        (acc << 32) | BigInt(word)
      }
    }
  }

  def forever(block: => Unit): Nothing = {
    while (true) {
      block
    }
    throw new Exception("Unreachable")
  }
  
}

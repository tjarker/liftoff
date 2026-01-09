package liftoff.misc

import java.io.File

class PathToFileOps(val path: String) extends AnyVal {
    def toFile: File = new File(path)
    def toDir: WorkingDirectory = WorkingDirectory(path)
  }

class BigIntOps(val x: BigInt) {
  def toWordArray: Array[Int] = {
    (0 until (x.bitLength + 31) / 32)
      .map { i =>
        (x >> (i * 32)).toInt
      }.toArray
  }
}

class WordArrayOps(val arr: Array[Int]) {
  def toBigInt: BigInt = {
    arr.foldRight(BigInt(0)) { (word, acc) =>
      (acc << 32) | BigInt(word)
    }
  }
}

trait Misc {

  import scala.language.implicitConversions

  implicit def pathToFileOps(path: String): PathToFileOps = new PathToFileOps(path)
  implicit def bigIntOps(x: BigInt): BigIntOps = new BigIntOps(x)
  implicit def wordArrayOps(arr: Array[Int]): WordArrayOps = new WordArrayOps(arr)
  

  def forever(block: => Unit): Nothing = {
    while (true) {
      block
    }
    throw new Exception("Unreachable")
  }
  
}

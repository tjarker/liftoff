package liftoff.chisel

import java.io.File

/** Verilog emission for Chisel 3.6: the Scala FIRRTL compiler writes one Verilog file for the
  * whole design and lists the black box sources it copied in `firrtl_black_box_resource_files.f`.
  */
private[chisel] object VerilogEmitter {

  /** Emits `gen`, whose top module is `name`, into `targetDir` and returns the files Verilator
    * has to compile.
    */
  def emit(name: String, gen: => chisel3.RawModule, targetDir: File): Seq[File] = {
    chisel3.emitVerilog(gen, Array("--target-dir", targetDir.getPath))

    val blackBoxList = new File(targetDir, "firrtl_black_box_resource_files.f")
    val blackBoxes =
      if (!blackBoxList.exists()) Seq()
      else {
        val source = scala.io.Source.fromFile(blackBoxList)
        try source.getLines().filter(_.nonEmpty).map(new File(_)).toSeq
        finally source.close()
      }
    new File(targetDir, s"$name.v") +: blackBoxes
  }
}

package liftoff.chisel

import java.io.File

/** Verilog emission for Chisel 7: firtool writes one SystemVerilog file per module and lists
  * them, with the black boxes, in `filelist.f`.
  */
private[chisel] object VerilogEmitter {

  /** Emits `gen`, whose top module is `name`, into `targetDir` and returns the files Verilator
    * has to compile.
    */
  def emit(name: String, gen: => chisel3.RawModule, targetDir: File): Seq[File] = {
    // firtool never removes the files of an older design, so start from an empty directory.
    if (targetDir.exists()) files(targetDir).foreach(_.delete())

    circt.stage.ChiselStage.emitSystemVerilogFile(
      gen,
      Array("--target-dir", targetDir.getPath, "--split-verilog")
    )

    val fileList = scala.io.Source.fromFile(new File(targetDir, "filelist.f"))
    val design =
      try
        fileList.getLines().filter(_.nonEmpty).map { line =>
          val file = new File(line)
          if (file.isAbsolute) file else new File(targetDir, line)
        }.toSeq
      finally fileList.close()

    // Layers are not in filelist.f. Chisel 7 puts assertions into the Verification layer, so
    // leaving its files out would silently disable them.
    design ++ files(targetDir).filter(_.getName.startsWith("layers-"))
  }

  private def files(file: File): Seq[File] =
    if (file.isDirectory) file.listFiles().toSeq.flatMap(files) else Seq(file)
}

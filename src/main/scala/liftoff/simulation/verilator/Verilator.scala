package liftoff.simulation.verilator

import java.io.File
import scala.util.{Try, Success, Failure}
import scala.collection.mutable.ListBuffer
import java.nio.file.Path
import liftoff.pathToFileOps
import liftoff.misc.WorkingDirectory

object Verilator {

  import scala.sys.process._

  import Arguments._

  sealed trait Argument {
    def toStrings: Seq[String] = this match {
      case CC                       => Seq("--cc")
      case EXE                      => Seq("--exe")
      case Main                     => Seq("--main")
      case Build                    => Seq("--build")
      case Help                     => Seq("--help")
      case Version                  => Seq("--version")
      case Assertions               => Seq("--assert")
      case Coverage                 => Seq("--coverage")
      case PublicFlatRW             => Seq("--public-flat-rw")
      case SystemVerilog            => Seq("-sv")
      case TraceVcd                 => Seq("--trace")
      case TraceFst                 => Seq("--trace-fst")
      case TraceSaif                => Seq("--trace-saif")
      case Timing                   => Seq("--timing")
      case NoTiming                 => Seq("--no-timing")
      case TopParam(name, value)    => Seq(s"-G$name=$value")
      case Include(path)            => Seq(s"-I$path")
      case Jobs(n)                  => Seq("-j", n.toString)
      case LdFlags(flags)           => Seq("-LDFLAGS", flags)
      case CFlags(flags)            => Seq("-CFLAGS", s"'$flags'")
      case OptimizationLevel(level) => Seq(s"-O$level")
      case TopModule(name)          => Seq("--top-module", name)
      case BuildDir(path)           => Seq("--Mdir", path)
      case CreateDpiLib(name)       => Seq("--lib-create", name)
      case DefaultTimeScale(scale)  => Seq("--timescale", scale)
      case OverrideTimeScale(scale) => Seq("--timescale-override", scale)
      case CustomFlag(flag)         => Seq(flag)
    }
  }

  /** Waveform/activity format Verilator was asked to emit. Determines which runtime object must be linked and which
    * tracer class the harness uses.
    */
  sealed trait TraceFormat {
    def runtimeObject: String
    def tracerClass: String
    def header: String

    /** The Verilator flag enabling this format, if any. */
    def argument: Option[Argument]
    def fileExtension: String
  }
  object TraceFormat {
    case object Vcd extends TraceFormat {
      val runtimeObject = "verilated_vcd_c.o"
      val tracerClass = "VerilatedVcdC"
      val header = "verilated_vcd_c.h"
      val argument = Some(TraceVcd)
      val fileExtension = "vcd"
    }
    case object Fst extends TraceFormat {
      val runtimeObject = "verilated_fst_c.o"
      val tracerClass = "VerilatedFstC"
      val header = "verilated_fst_c.h"
      val argument = Some(TraceFst)
      val fileExtension = "fst"
    }
    case object Saif extends TraceFormat {
      val runtimeObject = "verilated_saif_c.o"
      val tracerClass = "VerilatedSaifC"
      val header = "verilated_saif_c.h"
      val argument = Some(TraceSaif)
      val fileExtension = "saif"
    }
    case object NoTrace extends TraceFormat {
      val runtimeObject = ""
      val tracerClass = ""
      val header = ""
      val argument = None
      val fileExtension = ""
    }
  }

  object Arguments {
    case object CC extends Argument
    case object EXE extends Argument
    case object Main extends Argument
    case object Build extends Argument
    case object Help extends Argument
    case object Version extends Argument
    case object Assertions extends Argument
    case object Coverage extends Argument
    case object PublicFlatRW extends Argument
    case object SystemVerilog extends Argument
    case object TraceVcd extends Argument
    case object TraceFst extends Argument
    case object TraceSaif extends Argument
    case object Timing extends Argument
    case object NoTiming extends Argument
    case class TopParam(name: String, value: String) extends Argument
    case class Include(path: String) extends Argument
    case class Jobs(n: Int) extends Argument
    case class LdFlags(flags: String) extends Argument
    case class CFlags(flags: String) extends Argument
    case class OptimizationLevel(level: String) extends Argument
    case class TopModule(name: String) extends Argument
    case class BuildDir(path: String) extends Argument
    case class CreateDpiLib(name: String) extends Argument
    case class DefaultTimeScale(scale: String) extends Argument
    case class OverrideTimeScale(scale: String) extends Argument
    case class CustomFlag(flag: String) extends Argument
  }

  def getVersion: Option[String] =
    Verilator(Seq(Version), Seq())
      .map(_.trim.split(" ").apply(1))
      .toOption

  def getExecutable: Option[File] = {
    try {
      val path = Seq("which", "verilator").!!.trim
      Some(new File(path))
    } catch {
      case _: Throwable => None
    }
  }

  def getIncludeDir(): Try[Seq[File]] = {

    // find verilator bin using which
    val verilatorBin = getExecutable.getOrElse {
      return Failure(new Exception("Verilator executable not found."))
    }

    val base = (verilatorBin.getParentFile.getAbsolutePath() + "/../share/verilator/include").toFile

    if (!base.exists() || !base.isDirectory) {
      return Failure(new Exception(s"Verilator include directory not found: ${base.getAbsolutePath}"))
    }

    // the same directories verilated.mk passes; vltstd holds svdpi.h
    Success(Seq(base, new File(base, "vltstd")))
  }

  /** Runs `command`, a complete Verilator invocation building the model `name` into `dir`, and returns the object files
    * to link: the model, the Verilator runtime, the runtime of `trace` and one object per C++ file in `cppSources`.
    */
  def createRecipe(
      dir: WorkingDirectory,
      name: String,
      command: Seq[String],
      deps: Seq[File],
      trace: TraceFormat,
      cppSources: Seq[File]
  ): WorkingDirectory.Recipe[Seq[File]] = {

    val ext =
      if (System.getProperty("os.name").toLowerCase.contains("mac")) ".a"
      else ".o"

    val targets = Seq(
      dir / (s"V${name}__ALL" + ext),
      dir / "verilated.o",
      dir / "verilated_threads.o"
    ) ++ Option.when(trace != TraceFormat.NoTrace)(dir / trace.runtimeObject) ++
      cppSources.map(f => dir / (f.getName.stripSuffix(".cpp").stripSuffix(".cc") + ".o"))

    dir.addRecipe(
      targets,
      deps,
      command,
      identity
    )

  }

  def apply(args: Seq[Argument], files: Seq[File]): Try[String] = {

    val command = Seq("verilator") ++
      args.flatMap(_.toStrings) ++
      files.map(
        _.getAbsolutePath
      )

    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val logger = ProcessLogger(
      s => stdout.append(s + "\n"),
      s => stderr.append(s + "\n")
    )
    val exitCode = command.!(logger)
    if (exitCode == 0)
      Success(stdout.toString)
    else
      Failure(
        new Exception(
          s"Verilator failed with exit code $exitCode\n" +
            s"  Command: ${command.mkString(" ")}\n" +
            s"  Error: ${stderr.toString}"
        )
      )

  }
}

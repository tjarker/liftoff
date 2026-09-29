package liftoff.misc

import liftoff.simulation.Time
import liftoff.coroutine.CoroutineContextVariable

import scala.annotation.tailrec

/** Reports of a simulation, tagged with the simulation time and the reporting component.
  *
  * Each provider reports up to its level, `Info` by default. A level set for a provider applies to the providers below
  * it too, so `setLevel("root.env", Level.Debug)` covers `root.env.driver`. The environment variable `LIFTOFF_LOG` sets
  * the initial levels, for example `debug` or `info,liftoff.scheduler=trace`.
  */
object Reporting {

  /** How much to report, from nothing to everything. */
  sealed abstract class Level(val rank: Int) {
    def name: String = toString.toLowerCase
  }
  object Level {
    case object Off extends Level(0)
    case object Error extends Level(1)
    case object Warn extends Level(2)
    case object Info extends Level(3)
    case object Debug extends Level(4)
    case object Trace extends Level(5)

    val all: Seq[Level] = Seq(Off, Error, Warn, Info, Debug, Trace)

    def parse(name: String): Option[Level] = all.find(_.name == name.trim.toLowerCase)
  }

  /** The level of each provider: the level of its longest prefix in `prefixes`, else `default`. */
  case class Levels(default: Level, prefixes: Map[String, Level]) {

    def of(provider: String): Level = if (prefixes.isEmpty) default else lookup(provider)

    @tailrec private def lookup(provider: String): Level = prefixes.get(provider) match {
      case Some(level) => level
      case None        =>
        val dot = provider.lastIndexOf('.')
        if (dot < 0) default else lookup(provider.substring(0, dot))
    }

    def updated(provider: String, level: Level): Levels = copy(prefixes = prefixes.updated(provider, level))
  }

  object Levels {

    val default: Levels = Levels(Level.Info, Map.empty)

    /** Parses comma-separated `level` and `provider=level` entries, as in `info,liftoff.scheduler=trace`. */
    def parse(spec: String): Levels =
      spec.split(",").map(_.trim).filter(_.nonEmpty).foldLeft(default) { (levels, entry) =>
        def level(name: String) = Level.parse(name).getOrElse {
          throw new IllegalArgumentException(
            s"Unknown level '$name' in '$spec', use one of ${Level.all.map(_.name).mkString(", ")}"
          )
        }
        entry.split("=", 2) match {
          case Array(provider, name) => levels.updated(provider.trim, level(name))
          case Array(name)           => levels.copy(default = level(name))
        }
      }

    def fromEnvironment(): Levels = sys.env.get("LIFTOFF_LOG") match {
      case Some(spec) =>
        try parse(spec)
        catch {
          case e: IllegalArgumentException =>
            System.err.println(s"Ignoring LIFTOFF_LOG: ${e.getMessage}")
            default
        }
      case None => default
    }
  }

  val successTag = fansi.Color.Green("success")
  val errorTag = fansi.Color.Red("error")
  val warnTag = fansi.Color.Yellow("warn")
  val infoTag = fansi.Str("info")
  val debugTag = fansi.Color.Magenta("debug")
  val traceTag = fansi.Color.DarkGray("trace")

  val outputStream = new CoroutineContextVariable[java.io.PrintStream](System.out)
  val coloredOutput = new CoroutineContextVariable[Boolean](true)
  val providerName = new CoroutineContextVariable[String]("unknown")
  val levels = new CoroutineContextVariable[Levels](Levels.fromEnvironment())

  /** Sets the level of the providers without a level of their own. */
  def setLevel(level: Level): Unit = levels.value = levels.value.copy(default = level)

  /** Sets the level of `provider` and the providers below it. */
  def setLevel(provider: String, level: Level): Unit = levels.value = levels.value.updated(provider, level)

  /** Runs `block` with the level of the providers without a level of their own set to `level`. */
  def withLevel[R](level: Level)(block: => R): R = levels.withValue(levels.value.copy(default = level))(block)

  /** Runs `block` with the level of `provider` and the providers below it set to `level`. */
  def withLevel[R](provider: String, level: Level)(block: => R): R =
    levels.withValue(levels.value.updated(provider, level))(block)

  def isEnabled(provider: String, level: Level): Boolean = level.rank <= levels.value.of(provider).rank

  def withOutput[R](stream: java.io.PrintStream, colored: Boolean = true)(block: => R): R = {
    outputStream.withValue[R](stream) {
      coloredOutput.withValue[R](colored) {
        block
      }
    }
  }
  def setOutput(stream: java.io.PrintStream, colored: Boolean = true): Unit = {
    outputStream.value = stream
    coloredOutput.value = colored
  }

  def setProvider(name: String): Unit = {
    providerName.value = name
  }
  def getCurrentProvider(): String = {
    providerName.value
  }

  object NullStream
      extends java.io.PrintStream(new java.io.OutputStream {
        def write(b: Int): Unit = {}
      })

  def pathColor(str: String) = {
    str
      .split("\\.")
      .map(part => fansi.Color.LightMagenta(part).toString())
      .mkString(fansi.Color.LightGray(".").toString())
  }

  def reportStringColored(tag: fansi.Str, time: Option[Time], provider: String, message: String): String = {
    val tagStr = "[" + tag + "]" + ("─" * (7 - tag.length))
    val tagStrNoLine = "[" + tag + "]" + (" " * (7 - tag.length))
    val timeStrFmt = time match {
      case Some(t) if t.toString.endsWith("s ") => {
        val timeStr = t.toString.trim
        ("─" * (8 - timeStr.length)) + "@" + fansi.Color.True(51, 153, 255)(timeStr).toString() + "─"
      }
      case Some(t) => {
        val timeStr = t.toString
        ("─" * (9 - timeStr.length)) + "@" + fansi.Color.True(51, 153, 255)(timeStr).toString()
      }
      case None => "─" * 10
    }
    val providerStr = fansi.Color.LightGray("[").toString + pathColor(provider) + fansi.Color
      .LightGray("]")
      .toString() + ("─" * (25 - provider.length))
    val lines = message.split("\n")
    s"$tagStr─$timeStrFmt─$providerStr─╢ ${lines.mkString(s"\n" + (" " * 49) + "║ ")}\n" + " " * 49 + "║"
  }

  def reportString(tag: fansi.Str, time: Option[Time], provider: String, message: String): String = {
    if (coloredOutput.value) {
      reportStringColored(tag, time, provider, message)
    } else {
      val tagStr = "[" + tag.plainText + "]" + ("─" * (7 - tag.plainText.length))
      val timeStrFmt = time match {
        case Some(t) =>
          val timeStr = t.toString
          ("─" * (9 - timeStr.length)) + "@" + timeStr
        case None => "─" * 10
      }
      val providerStr = "[" + provider + "]" + ("─" * (25 - provider.length))
      val lines = message.split("\n")
      s"$tagStr─$timeStrFmt─$providerStr─╢ ${lines.mkString(s"\n" + (" " * 49) + "║ ")}\n" + " " * 49 + "║"
    }
  }

  private def report(level: Level, tag: fansi.Str, time: => Option[Time], provider: String, message: => String): Unit =
    if (isEnabled(provider, level)) outputStream.value.println(reportString(tag, time, provider, message))

  def infoStr(time: Option[Time], provider: String, message: => String): String = {
    reportString(infoTag, time, provider, message)
  }
  def info(time: => Option[Time], provider: String, message: => String): Unit = {
    report(Level.Info, infoTag, time, provider, message)
  }
  def info(time: => Option[Time], message: => String): Unit = {
    info(time, providerName.value, message)
  }

  def warnStr(time: Option[Time], provider: String, message: => String): String = {
    reportString(warnTag, time, provider, message)
  }
  def warn(time: => Option[Time], provider: String, message: => String): Unit = {
    report(Level.Warn, warnTag, time, provider, message)
  }
  def warn(time: => Option[Time], message: => String): Unit = {
    warn(time, providerName.value, message)
  }

  def errorStr(time: Option[Time], provider: String, message: => String): String = {
    reportString(errorTag, time, provider, message)
  }
  def error(time: => Option[Time], provider: String, message: => String): Unit = {
    report(Level.Error, errorTag, time, provider, message)
  }
  def error(time: => Option[Time], message: => String): Unit = {
    error(time, providerName.value, message)
  }

  def successStr(time: Option[Time], provider: String, message: => String): String = {
    reportString(successTag, time, provider, message)
  }
  def success(time: => Option[Time], provider: String, message: => String): Unit = {
    report(Level.Info, successTag, time, provider, message)
  }
  def success(time: => Option[Time], message: => String): Unit = {
    success(time, providerName.value, message)
  }

  def debugStr(time: Option[Time], provider: String, message: => String): String = {
    reportString(debugTag, time, provider, message)
  }
  def debug(time: => Option[Time], provider: String, message: => String): Unit = {
    report(Level.Debug, debugTag, time, provider, message)
  }
  def debug(time: => Option[Time], message: => String): Unit = {
    debug(time, providerName.value, message)
  }

  def traceStr(time: Option[Time], provider: String, message: => String): String = {
    reportString(traceTag, time, provider, message)
  }
  def trace(time: => Option[Time], provider: String, message: => String): Unit = {
    report(Level.Trace, traceTag, time, provider, message)
  }
  def trace(time: => Option[Time], message: => String): Unit = {
    trace(time, providerName.value, message)
  }

  // inspired by https://stackoverflow.com/a/55143951
  def table(table: Seq[Seq[Any]]): String = {
    if (table.isEmpty) ""
    else {
      // Get column widths based on the maximum cell width in each column (+2 for a one character padding on each side)
      val colWidths = table.transpose.map(
        _.map(cell => if (cell == null) 0 else cell.toString.length).max + 2
      )
      // Format each row
      val rows = table.map(
        _.zip(colWidths)
          .map { case (item, size) => (" %-" + (size - 1) + "s").format(item) }
          .mkString("║", "║", "║")
      )
      // Formatted separator row, used to separate the header and draw table borders
      val middleSeparator = colWidths.map("═" * _).mkString("╠", "╬", "╣")
      val topSeperator = colWidths.map("═" * _).mkString("╔", "╦", "╗")
      val bottomSeperator = colWidths.map("═" * _).mkString("╚", "╩", "╝")
      // Put the table together and return
      (topSeperator +: rows.head +: middleSeparator +: rows.tail :+ bottomSeperator)
        .mkString("\n", "\n", "\n")
    }
  }

  def tableWithHeader(header: Seq[Any], rows: Seq[Seq[Any]]): String = {
    table(header +: rows)
  }

  def showBanner(): Unit = {
    if (coloredOutput.value) {
      outputStream.value.println(fansi.Str(liftoffBanner).plainText)
    } else {
      outputStream.value.println(liftoffBanner)
    }
  }

  def liftoffBanner: String = {
    import Console._
    s"""
${BLUE}╔════════════════════════════════════════════════════════════════════════════════╗${RESET}
${BLUE}║${RESET} ${YELLOW}██╗     ██╗███████╗████████╗ ██████╗ ███████╗███████╗${RESET}          ${YELLOW}__|__${RESET}           ${BLUE}║${RESET} 
${BLUE}║${RESET} ${YELLOW}██║     ██║██╔════╝╚══██╔══╝██╔═══██╗██╔════╝██╔════╝${RESET}    ${YELLOW}--@--@--(_)--@--@--${RESET}   ${BLUE}║${RESET} 
${BLUE}║${RESET} ${YELLOW}██║     ██║█████╗     ██║   ██║   ██║█████╗  █████╗  ${RESET}         ${BLUE}\\   \\\\   \\${RESET}       ${BLUE}║${RESET} 
${BLUE}║${RESET} ${YELLOW}██║     ██║██╔══╝     ██║   ██║   ██║██╔══╝  ██╔══╝  ${RESET}          ${BLUE}\\   \\\\   \\${RESET}      ${BLUE}║${RESET} 
${BLUE}║${RESET} ${YELLOW}███████╗██║██║        ██║   ╚██████╔╝██║     ██║     ${RESET}           ${BLUE}\\   \\\\   \\${RESET}     ${BLUE}║${RESET} 
${BLUE}║${RESET} ${YELLOW}╚══════╝╚═╝╚═╝        ╚═╝    ╚═════╝ ╚═╝     ╚═╝     ${RESET}            ${BLUE}\\   \\\\   \\${RESET}    ${BLUE}║${RESET} 
${BLUE}╚════════════════════════════════════════════════════════════════════════════════╝${RESET}
"""
  }

}

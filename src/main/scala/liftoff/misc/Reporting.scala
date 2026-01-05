package liftoff.misc

import liftoff.simulation.Time
import liftoff.coroutine.CoroutineContextVariable

object Reporting {

  val successTag = fansi.Color.Green("success")
  val errorTag   = fansi.Color.Red("error")
  val warnTag    = fansi.Color.Yellow("warn")
  val infoTag    = fansi.Str("info")
  val debugTag   = fansi.Color.Magenta("debug")


  val outputStream = new CoroutineContextVariable[java.io.PrintStream](System.out)
  val coloredOutput = new CoroutineContextVariable[Boolean](true)
  val providerName = new CoroutineContextVariable[String]("unknown")
  val providerFilters = new CoroutineContextVariable[Set[String]](Set())

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

  def addProviderFilter(filter: String): Unit = {
    val filters = providerFilters.value
    providerFilters.value = filters + filter
  }
  def removeProviderFilter(filter: String): Unit = {
    val filters = providerFilters.value
    providerFilters.value = filters - filter
  }

  object NullStream extends java.io.PrintStream(new java.io.OutputStream {
    def write(b: Int): Unit = {}
  })

  def pathColor(str: String) = {
    str.split("\\.").map(part =>fansi.Color.LightMagenta(part).toString()).mkString(fansi.Color.LightGray(".").toString())
  }

  def reportStringColored(tag: fansi.Str, time: Option[Time], provider: String, message: String): String = {
    val tagStr = "[" + tag + "]" + ("─" * (7-tag.length))
    val tagStrNoLine = "[" + tag + "]" + (" " * (7-tag.length))
    val timeStrFmt = time match {
      case Some(t) if t.toString.endsWith("s ") => {
        val timeStr = t.toString.trim
        ("─" * (8 - timeStr.length)) + "@" + fansi.Color.LightBlue(timeStr).toString() + "─"
      }
      case Some(t) => {
        val timeStr = t.toString
        ("─" * (9 - timeStr.length)) + "@" + fansi.Color.True(51,153,255)(timeStr).toString()
      }
      case None => "─" * 10
    }
    val providerStr = fansi.Color.LightGray("[").toString + pathColor(provider) + fansi.Color.LightGray("]").toString() + ("─" * (25 - provider.length))
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

  def infoStr(time: Option[Time], provider: String, message: String): String = {
    reportString(infoTag, time, provider, message)
  }
  def info(time: Option[Time], provider: String, message: String): Unit = {
    if (!providerFilters.value.contains(provider)) outputStream.value.println(infoStr(time, provider, message))
  }
  def info(time: Option[Time], message: String): Unit = {
    info(time, providerName.value, message)
  }

  def warnStr(time: Option[Time], provider: String, message: String): String = {
    reportString(warnTag, time, provider, message)
  }
  def warn(time: Option[Time], provider: String, message: String): Unit = {
    if (!providerFilters.value.contains(provider)) outputStream.value.println(warnStr(time, provider, message))
  }
  def warn(time: Option[Time], message: String): Unit = {
    warn(time, providerName.value, message)
  }

  def errorStr(time: Option[Time], provider: String, message: String): String = {
    reportString(errorTag, time, provider, message)
  }
  def error(time: Option[Time], provider: String, message: String): Unit = {
    if (!providerFilters.value.contains(provider)) outputStream.value.println(errorStr(time, provider, message))
  }
  def error(time: Option[Time], message: String): Unit = {
    error(time, providerName.value, message)
  }

  def successStr(time: Option[Time], provider: String, message: String): String = {
    reportString(successTag, time, provider, message)
  }
  def success(time: Option[Time], provider: String, message: String): Unit = {
    if (!providerFilters.value.contains(provider)) outputStream.value.println(successStr(time, provider, message))
  }
  def success(time: Option[Time], message: String): Unit = {
    success(time, providerName.value, message)
  }

  def debugStr(time: Option[Time], provider: String, message: String): String = {
    reportString(debugTag, time, provider, message)
  }
  def debug(time: Option[Time], provider: String, message: String): Unit = {
    if (!providerFilters.value.contains(provider)) outputStream.value.println(debugStr(time, provider, message))
  }
  def debug(time: Option[Time], message: String): Unit = {
    debug(time, providerName.value, message)
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

package liftoff.misc



import liftoff.simulation.Time

object Reporting {

  val successTag = fansi.Color.Green("success")
  val errorTag   = fansi.Color.Red("error")
  val warnTag    = fansi.Color.Yellow("warn")
  val infoTag    = fansi.Str("info")
  val debugTag   = fansi.Color.Magenta("debug")


  def pathColor(str: String) = {
    str.split("\\.").map(part =>fansi.Color.LightMagenta(part).toString()).mkString(fansi.Color.LightGray(".").toString())
  }

  def reportString(tag: fansi.Str, time: Time, provider: String, message: String): String = {
    val tagStr = "[" + tag + "]" + (" " * (7-tag.length))
    val timeStr = time.toString()
    val timeStrFmt = (" " * (9 - timeStr.length)) + "@" + fansi.Color.LightRed(timeStr).toString()
    val providerStr = fansi.Color.LightGray("[").toString + pathColor(provider) + fansi.Color.LightGray("]").toString() + (" " * (25 - provider.length))
    s"$tagStr $timeStrFmt $providerStr $message"
  }

  def infoStr(time: Time, provider: String, message: String): String = {
    reportString(infoTag, time, provider, message)
  }
  def info(time: Time, provider: String, message: String): Unit = {
    println(infoStr(time, provider, message))
  }

  def warnStr(time: Time, provider: String, message: String): String = {
    reportString(warnTag, time, provider, message)
  }
  def warn(time: Time, provider: String, message: String): Unit = {
    println(warnStr(time, provider, message))
  }

  def errorStr(time: Time, provider: String, message: String): String = {
    reportString(errorTag, time, provider, message)
  }
  def error(time: Time, provider: String, message: String): Unit = {
    println(errorStr(time, provider, message))
  }

  def successStr(time: Time, provider: String, message: String): String = {
    reportString(successTag, time, provider, message)
  }
  def success(time: Time, provider: String, message: String): Unit = {
    println(successStr(time, provider, message))
  }

  def debugStr(time: Time, provider: String, message: String): String = {
    reportString(debugTag, time, provider, message)
  }
  def debug(time: Time, provider: String, message: String): Unit = {
    println(debugStr(time, provider, message))
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

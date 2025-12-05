package liftoff.simulation.verilator

import java.io.File

object PortCollector {

  import scala.util.matching.Regex

  class SymbolId {
    private var id: Long = 0L
    def next(): Long = {
      val currentId = id
      id += 1
      currentId
    }
  }

  class Matcher[T <: VerilatorPortDescriptor](regex: Regex, builder: (Seq[String], Long) => T) {
    def tryMatch(line: String, idGen: SymbolId): Option[T] = {
      regex
        .findFirstMatchIn(line)
        .map(m => builder(m.subgroups, idGen.next()))
    }
  }

  val iden = raw"(\w+)"
  val varName = raw"&(\w+)"
  val num = raw"(\d+)"
  def definition(name: String, parts: Seq[String]): Regex = 
    raw"""$name\(${parts.mkString(",")}\);""".r


  /* 
  
  # define VL_IN8(name, msb,lsb)		CData name		///< Declare input signal, 1-8 bits
  # define VL_IN16(name, msb,lsb)		SData name		///< Declare input signal, 9-16 bits
  # define VL_IN64(name, msb,lsb)		QData name		///< Declare input signal, 33-64 bits
  # define VL_IN(name, msb,lsb)		IData name		///< Declare input signal, 17-32 bits
  # define VL_INW(name, msb,lsb, words)	WData name[words]	///< Declare input signal, 65+ bits
  # define VL_INOUT8(name, msb,lsb)	CData name		///< Declare bidir signal, 1-8 bits
  # define VL_INOUT16(name, msb,lsb)	SData name		///< Declare bidir signal, 9-16 bits
  # define VL_INOUT64(name, msb,lsb)	QData name		///< Declare bidir signal, 33-64 bits
  # define VL_INOUT(name, msb,lsb)	IData name		///< Declare bidir signal, 17-32 bits
  # define VL_INOUTW(name, msb,lsb, words) WData name[words]	///< Declare bidir signal, 65+ bits
  # define VL_OUT8(name, msb,lsb)		CData name		///< Declare output signal, 1-8 bits
  # define VL_OUT16(name, msb,lsb)	SData name		///< Declare output signal, 9-16 bits
  # define VL_OUT64(name, msb,lsb)	QData name		///< Declare output signal, 33-64bits
  # define VL_OUT(name, msb,lsb)		IData name		///< Declare output signal, 17-32 bits
  # define VL_OUTW(name, msb,lsb, words)	WData name[words]	///< Declare output signal, 65+ bits

  */

  val narrowInputMatcher = new Matcher[VerilatorInputDescriptor](
    definition(raw"VL_IN\d*", Seq(varName, num, num)),
    (parts, id) => {
      val name = parts(0)
      val msb = parts(1).toInt
      val lsb = parts(2).toInt
      val width = msb - lsb + 1
      VerilatorInputDescriptor(name, id.toInt, width)
    }
  )

  val wideInputMatcher = new Matcher[VerilatorInputDescriptor](
    definition(raw"VL_INW", Seq(varName, num, num, num)),
    (parts, id) => {
      val name = parts(0)
      val msb = parts(1).toInt
      val lsb = parts(2).toInt
      val width = msb - lsb + 1
      VerilatorInputDescriptor(name, id.toInt, width)
    }
  )

  val narrowOutputMatcher = new Matcher[VerilatorOutputDescriptor](
    definition(raw"VL_OUT\d*", Seq(varName, num, num)),
    (parts, id) => {
      val name = parts(0)
      val msb = parts(1).toInt
      val lsb = parts(2).toInt
      val width = msb - lsb + 1
      VerilatorOutputDescriptor(name, id.toInt, width)
    }
  )

  val wideOutputMatcher = new Matcher[VerilatorOutputDescriptor](
    definition(raw"VL_OUTW", Seq(varName, num, num, num)),
    (parts, id) => {
      val name = parts(0)
      val msb = parts(1).toInt
      val lsb = parts(2).toInt
      val width = msb - lsb + 1
      VerilatorOutputDescriptor(name, id.toInt, width)
    }
  )

  val allMatchers: Seq[Matcher[_ <: VerilatorPortDescriptor]] = Seq(
    narrowInputMatcher,
    wideInputMatcher,
    narrowOutputMatcher,
    wideOutputMatcher
  )

  def collectPorts(lines: Seq[String]): Seq[VerilatorPortDescriptor] = {
    val idGen = new SymbolId()

    lines.flatMap { line =>
      allMatchers
        .flatMap(_.tryMatch(line, idGen))
    }
  }

  def collectPorts(file: File): Seq[VerilatorPortDescriptor] = {
    val source = scala.io.Source.fromFile(file)
    val lines = source.getLines().toSeq
    source.close()

    collectPorts(lines)
  }
}

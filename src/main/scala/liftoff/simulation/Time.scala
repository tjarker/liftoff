package liftoff.simulation

object Time {

  abstract class TimeUnit(val exp: Int)
  object TimeUnit {
    case object fs extends TimeUnit(0)
    case object ps extends TimeUnit(3)
    case object ns extends TimeUnit(6)
    case object us extends TimeUnit(9)
    case object ms extends TimeUnit(12)
    case object s extends TimeUnit(15)

    def fromExp(exp: Int): TimeUnit = {
      exp match {
        case 0  => fs
        case 3  => ps
        case 6  => ns
        case 9  => us
        case 12 => ms
        case 15 => s
        case _  => throw new IllegalArgumentException(s"Invalid time unit exponent: $exp")
      }
    }
  }

  implicit class IntToTime(i: Int) {
    def fs: Time = Time(i, TimeUnit.fs)
    def ps: Time = Time(i, TimeUnit.ps)
    def ns: Time = Time(i, TimeUnit.ns)
    def us: Time = Time(i, TimeUnit.us)
    def ms: Time = Time(i, TimeUnit.ms)
    def s: Time = Time(i, TimeUnit.s)
  }

  implicit class LongToTime(l: Long) {
    def fs: Time = Time(l, TimeUnit.fs)
    def ps: Time = Time(l, TimeUnit.ps)
    def ns: Time = Time(l, TimeUnit.ns)
    def us: Time = Time(l, TimeUnit.us)
    def ms: Time = Time(l, TimeUnit.ms)
    def s: Time = Time(l, TimeUnit.s)
  }

  implicit class BigIntToTime(b: BigInt) {
    def fs: Time = Time(b.toLong, TimeUnit.fs)
    def ps: Time = Time(b.toLong, TimeUnit.ps)
    def ns: Time = Time(b.toLong, TimeUnit.ns)
    def us: Time = Time(b.toLong, TimeUnit.us)
    def ms: Time = Time(b.toLong, TimeUnit.ms)
    def s: Time = Time(b.toLong, TimeUnit.s)
  }

  def apply(value: Long, unit: TimeUnit): Time = {
    val t = new Time
    t.valueFs = value * math.pow(10, unit.exp).toLong
    t
  }

  class AbsoluteTime extends Time
  class RelativeTime extends Time

}

class Time(private[liftoff] var valueFs: Long = 0) extends Ordered[Time] {

  import Time.TimeUnit

  def +(that: Time): Time = {
    Time(valueFs + that.valueFs, TimeUnit.fs)
  }
  def -(that: Time): Time = {
    Time(valueFs - that.valueFs, TimeUnit.fs)
  }
  def *(that: Long): Time = {
    Time(valueFs * that, TimeUnit.fs)
  }
  def /(that: Time): Long = {
    valueFs / that.valueFs
  }
  def /(that: Long): Time = {
    Time(valueFs / that, TimeUnit.fs)
  }

  def toString(unit: TimeUnit): String = {
    val exp = unit.exp
    val v = valueFs / math.pow(10, exp)
    s"%.3f%-2s".format(v, unit.toString)
  }

  private def findBestTimeUnit(t: Time, unit: TimeUnit): TimeUnit = {
    val v = valueFs / math.pow(10, unit.exp)
    if (unit == TimeUnit.s || v < 1000) {
      unit
    } else {
      findBestTimeUnit(t, TimeUnit.fromExp(unit.exp + 3))
    }
  }

  override def toString(): String = {
    toString(findBestTimeUnit(this, TimeUnit.fs))
  }

  def valueIn(unit: Time): Long = {
    valueFs / unit.valueFs
  }

  override def compare(that: Time): Int = {
    this.valueFs.compare(that.valueFs)
  }

  def ==(that: Time): Boolean = {
    this.valueFs == that.valueFs
  }
  def !=(that: Time): Boolean = {
    this.valueFs != that.valueFs
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Time => this.valueFs == that.valueFs
      case _          => false
    }
  }

  def fs: Long = valueFs
  def ps: Long = valueFs / 1000
  def ns: Long = valueFs / 1000000
  def us: Long = valueFs / 1000000000
  def ms: Long = valueFs / 1000000000000L
  def s: Long = valueFs / 1000000000000000L

  def absolute: Time.AbsoluteTime = {
    val t = new Time.AbsoluteTime
    t.valueFs = valueFs
    t
  }

  def relative: Time.RelativeTime = {
    val t = new Time.RelativeTime
    t.valueFs = valueFs
    t
  }

}
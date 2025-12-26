package liftoff.simulation.task


object Region {
  object Default extends Region(0)
  object Monitor extends Region(Int.MaxValue)
}
case class Region(id: Int)

package liftoff.verify.component

import liftoff.coroutine.Gen
import liftoff.verify.ReceiverPort
import liftoff.coroutine.BiGen

trait Drives[T, F] {
  def drive(gen: BiGen[F, T]): DriveCompletion
}

trait Monitors[T] {
  def addSubscriber(c: AnalysisComponent[T]): Unit
}

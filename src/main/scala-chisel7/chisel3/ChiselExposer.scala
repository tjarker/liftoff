package chisel3

import scala.annotation.nowarn

/** Access to Chisel internals, for Chisel 7. Chisel 7 moved the bindings into
  * `chisel3.internal.binding`.
  */
object ChiselExposer {

  def enumFactory(enumType: EnumType): ChiselEnum = {
    enumType.factory
  }

  /** Whether `data` is bound to `DontCare`, such as an unset field of a partial literal. */
  @nowarn("msg=deprecated")
  def isDontCare(data: Data): Boolean = {
    data.topBindingOpt == Some(internal.binding.DontCareBinding())
  }

}

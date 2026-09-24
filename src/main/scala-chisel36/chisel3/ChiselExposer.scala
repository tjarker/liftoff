package chisel3

import scala.annotation.nowarn

/** Access to Chisel internals, for Chisel 3.6. */
object ChiselExposer {

  def enumFactory(enumType: EnumType): ChiselEnum = {
    enumType.factory
  }

  /** Whether `data` is bound to `DontCare`, such as an unset field of a partial literal. */
  @nowarn("msg=deprecated")
  def isDontCare(data: Data): Boolean = {
    data.topBindingOpt == Some(internal.DontCareBinding())
  }

}

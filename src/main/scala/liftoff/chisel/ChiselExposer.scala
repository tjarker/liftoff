package chisel3

import scala.annotation.nowarn

object ChiselExposer {

  def enumFactory(enumType: EnumType): ChiselEnum = {
    enumType.factory
  }

  @nowarn("msg=deprecated")
  def topBindingOpt(data: Data): Option[internal.TopBinding] = {
    data.topBindingOpt
  }

  @nowarn("msg=deprecated")
  def dontCareBinding(): internal.DontCareBinding = {
    internal.DontCareBinding()
  }

}


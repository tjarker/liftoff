package chisel3

object ChiselExposer {

  def enumFactory(enumType: EnumType): ChiselEnum = {
    enumType.factory
  }

  def topBindingOpt(data: Data): Option[internal.TopBinding] = {
    data.topBindingOpt
  }

  def dontCareBinding(): internal.DontCareBinding = {
    internal.DontCareBinding()
  }

}


package metal

trait FColl {

  type IType <: IColl

  type MType <: MColl

  def mutableCopy: MType

}

trait IColl extends FColl

trait MColl extends FColl {

  var immutable: Boolean = false

  def result(): IType

}

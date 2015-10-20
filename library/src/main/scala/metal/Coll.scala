package metal

trait FColl {

  type IType <: IColl

  type MType <: MColl

  def mutableCopy(): MType

}

trait IColl extends FColl

trait MColl extends FColl {

  def result(): IType

}

package metal

/** Trait defining a container's enumeration element. */
trait Elements[E] extends Pointable {

  def ptrElement[@specialized F](ptr: VPtr[Tag]): F

}

/** Containers that are composed of keys (ex: sets). */
trait ElementsK[K] extends Elements[K] with Keys[K] {

  def ptrElement[@specialized L](ptr: VPtr[Tag]): L = ptrKey[L](ptr)

}

/** Containers that are composed of key-value pairs (ex: maps). */
trait ElementsKV[K, V] extends Elements[(K, V)] with Keys[K] with Values[V] {

  def ptrElement[@specialized F](ptr: VPtr[Tag]): F =
    (ptrKey[K](ptr), ptrValue[V](ptr)).asInstanceOf[F]

}

/** Containers that are composed of values (i.e. sequences). */
trait ElementsV[V] extends Elements[V] with Values[V] {

  def ptrElement[@specialized W](ptr: VPtr[Tag]): W = ptrValue[W](ptr)

}

/** Containers that are composed of key and two value types. */
trait ElementsKV1V2[K, V1, V2] extends Elements[(K, V1, V2)] with Keys[K] with Values1[V1] with Values2[V2] {

  def ptrElement[@specialized F](ptr: VPtr[Tag]): F =
    (ptrKey[K](ptr), ptrValue1[V1](ptr), ptrValue2[V2](ptr)).asInstanceOf[F]

}

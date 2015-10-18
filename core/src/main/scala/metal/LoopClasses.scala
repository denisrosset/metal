package metal

import spire.util.Opt

final class OpsK[K](val lhs: ShapeK with Keys[K] with Nextable with Countable) {

  def foreach(body: K => Unit): Unit = macro macros.Foreach.k[K]

}

final class OpsV[V](val lhs: Values[V] with Nextable with Countable) {

  def foreach(body: V => Unit): Unit = macro macros.Foreach.v[V]

}

final class OpsKV[K, V](val lhs: ShapeKV with Keys[K] with Values[V] with Nextable with Countable) {

  def foreach(body: (K, V) => Unit): Unit = macro macros.Foreach.kv[K, V]

}

final class OpsKV1V2[K, V1, V2](val lhs: ShapeKV1V2 with Keys[K] with Values1[V1] with Values2[V2] with Nextable with Countable) {

  def foreach(body: (K, V1, V2) => Unit): Unit = macro macros.Foreach.kv1v2[K, V1, V2]

}

package metal
package macros

import spire.macros.compat.Context

import MacroUtils._

object VPtrOps {

  def element1[C <: Elements1[E1] with Singleton:c.WeakTypeTag, E1:c.WeakTypeTag](c: Context): c.Expr[E1] = {
    import c.universe._
    val lhs = c.prefix.tree
    val tagE1 = implicitly[c.WeakTypeTag[E1]]
    val container = extractSingleton[C](c)
    c.Expr[E1](q"$container.ptrElement1[$tagE1](_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

  def element2[C <: Elements2[E2] with Singleton:c.WeakTypeTag, E2:c.WeakTypeTag](c: Context): c.Expr[E2] = {
    import c.universe._
    val lhs = c.prefix.tree
    val tagE2 = implicitly[c.WeakTypeTag[E2]]
    val container = extractSingleton[C](c)
    c.Expr[E2](q"$container.ptrElement2[$tagE2](_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

  def element3[C <: Elements3[E3] with Singleton:c.WeakTypeTag, E3:c.WeakTypeTag](c: Context): c.Expr[E3] = {
    import c.universe._
    val lhs = c.prefix.tree
    val tagE3 = implicitly[c.WeakTypeTag[E3]]
    val container = extractSingleton[C](c)
    c.Expr[E3](q"$container.ptrElement3[$tagE3](_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

  def key[C <: Keys[K] with Singleton:c.WeakTypeTag, K:c.WeakTypeTag](c: Context): c.Expr[K] = {
    import c.universe._
    val lhs = c.prefix.tree
    val tagK = implicitly[c.WeakTypeTag[K]]
    val container = extractSingleton[C](c)
    c.Expr[K](q"$container.ptrKey[$tagK](_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

  def next[C <: Nextable with Singleton:c.WeakTypeTag](c: Context): c.Expr[Ptr[C]] = {
    import c.universe._
    val tagC = implicitly[c.WeakTypeTag[C]]
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    c.Expr[Ptr[C]](q"_root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($lhs.raw)).raw)")
  }

  def remove[C <: Removable with Singleton:c.WeakTypeTag](c: Context)(): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    c.Expr[Unit](q"$container.ptrRemove(_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

  def removeAndAdvance[C <: Removable with Singleton:c.WeakTypeTag](c: Context)(): c.Expr[Ptr[C]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    c.Expr[Ptr[C]](q"_root_.metal.Ptr[$tagC]($container.ptrRemoveAndAdvance(_root_.metal.VPtr[$container.type]($lhs.raw)).raw)")
  }

  def update[C <: Updatable[V] with Singleton:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(newValue: c.Expr[V]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagV = implicitly[c.WeakTypeTag[V]]
    c.Expr[Unit](q"$container.ptrUpdate[$tagV](_root_.metal.VPtr[$container.type]($lhs.raw), $newValue)")
  }

  def update1[C <: Updatable1[V1] with Singleton:c.WeakTypeTag, V1:c.WeakTypeTag](c: Context)(newValue1: c.Expr[V1]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagV1 = implicitly[c.WeakTypeTag[V1]]
    c.Expr[Unit](q"$container.ptrUpdate1[$tagV1](_root_.metal.VPtr[$container.type]($lhs.raw), $newValue1)")
  }

  def update2[C <: Updatable2[V2] with Singleton:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(newValue2: c.Expr[V2]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagV2 = implicitly[c.WeakTypeTag[V2]]
    c.Expr[Unit](q"$container.ptrUpdate2[$tagV2](_root_.metal.VPtr[$container.type]($lhs.raw), $newValue2)")
  }

  def value[C <: Values[V] with Singleton:c.WeakTypeTag, V:c.WeakTypeTag](c: Context): c.Expr[V] = {
    import c.universe._
    val lhs = c.prefix.tree
    val vType = implicitly[c.WeakTypeTag[V]]
    val container = extractSingleton[C](c)
    c.Expr[V](q"$container.ptrValue[$vType](_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

  def value1[C <: Values1[V1] with Singleton:c.WeakTypeTag, V1:c.WeakTypeTag](c: Context): c.Expr[V1] = {
    import c.universe._
    val lhs = c.prefix.tree
    val v1Type = implicitly[c.WeakTypeTag[V1]]
    val container = extractSingleton[C](c)
    c.Expr[V1](q"$container.ptrValue1[$v1Type](_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

  def value2[C <: Values2[V2] with Singleton:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context): c.Expr[V2] = {
    import c.universe._
    val lhs = c.prefix.tree
    val v2Type = implicitly[c.WeakTypeTag[V2]]
    val container = extractSingleton[C](c)
    c.Expr[V2](q"$container.ptrValue2[$v2Type](_root_.metal.VPtr[$container.type]($lhs.raw))")
  }

}

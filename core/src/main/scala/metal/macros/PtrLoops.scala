package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._

import MacroUtils._

object PtrLoops {

  def foreach[C <: Nextable with Singleton:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => Unit]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp) = util.names("ptr", "vp")
    val tree = q"""
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  $body($vp)
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def count[C <: Nextable with Singleton:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => Boolean]): c.Expr[Int] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, cnt) = util.names("ptr", "vp", "cnt")
    val tree = q"""
var $ptr =  _root_.metal.Ptr[$tagC]($lhs.raw)
var $cnt: Int = 0
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  if ($body($vp)) $cnt += 1
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$cnt
"""
    new InlineUtil[c.type](c).inlineAndReset[Int](tree)
  }

  def forall[C <: Nextable with Singleton:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
var $res: Boolean = true
while ($res && $ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  $res = $body($vp)
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def exists[C <: Nextable with Singleton:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
var $res: Boolean = false
while (!$res && $ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  $res = $body($vp)
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def foldLeft[C <: Nextable with Singleton:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(z: c.Expr[A])(body: c.Expr[(A, VPtr[C]) => A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
var $res: $tagA = $z
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  $res = $body($res, $vp)
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def maxBy[C <: Nextable with Singleton:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => A])(orderA: c.Expr[Order[A]]): c.Expr[Ptr[C]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(first, ptr, maxPtr, vp, a, maxA) = util.names("first", "ptr", "maxPtr", "vp", "a", "maxA")
    val tree = q"""
var $first: Boolean = true
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
var $maxPtr = _root_.metal.Ptr[$tagC](-1L)
var $maxA: $tagA = null.asInstanceOf[$tagA]
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  val $a: $tagA = $body($vp)
  if ($first || $orderA.gt($a, $maxA)) {
    $maxA = $a
    $maxPtr = $ptr
    $first = false
  }
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$maxPtr
"""
    new InlineUtil[c.type](c).inlineAndReset[Ptr[C]](tree)
  }

  def minBy[C <: Nextable with Singleton:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => A])(orderA: c.Expr[Order[A]]): c.Expr[Ptr[C]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(first, ptr, minPtr, vp, a, minA) = util.names("first", "ptr", "minPtr", "vp", "a", "minA")
    val tree = q"""
var $first: Boolean = true
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
var $minPtr = _root_.metal.Ptr[$tagC](-1L)
var $minA: $tagA = null.asInstanceOf[$tagA]
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  val $a: $tagA = $body($vp)
  if ($first || $orderA.lt($a, $minA)) {
    $minA = $a
    $minPtr = $ptr
    $first = false
  }
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$minPtr
"""
    new InlineUtil[c.type](c).inlineAndReset[Ptr[C]](tree)
  }

  def sumBy[C <: Nextable with Singleton:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => A])(am: c.Expr[AdditiveMonoid[A]]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
var $res: $tagA = $am.zero
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  $res = $am.plus($res, $body($vp))
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def productBy[C <: Nextable with Singleton:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[C] => A])(mm: c.Expr[MultiplicativeMonoid[A]]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractSingleton[C](c)
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
var $ptr = _root_.metal.Ptr[$tagC]($lhs.raw)
var $res: $tagA = $mm.one
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$tagC]($ptr.raw)
  $res = $mm.times($res, $body($vp))
  $ptr = _root_.metal.Ptr[$tagC]($container.ptrNext(_root_.metal.VPtr[$container.type]($vp.raw)).raw)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

}

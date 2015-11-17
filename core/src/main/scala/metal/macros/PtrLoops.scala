package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._

import MacroUtils._

object PtrLoops {

  def foreach[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => Unit]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp) = util.names("ptr", "vp")
    val tree = q"""
{
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    $body($vp)
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def count[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => Boolean]): c.Expr[Int] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, cnt) = util.names("ptr", "vp", "cnt")
    val tree = q"""
{
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  var $cnt: Int = 0
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    if ($body($vp)) $cnt += 1
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $cnt
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Int](tree)
  }

  def forall[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  var $res: Boolean = true
  while ($res && $ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    $res = $body($vp)
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def exists[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => Boolean]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  var $res: Boolean = false
  while (!$res && $ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    $res = $body($vp)
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def foldLeft[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(z: c.Expr[A])(body: c.Expr[(A, VPtr[T, C]) => A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  var $res: $tagA = $z
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    $res = $body($res, $vp)
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def maxBy[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => A])(orderA: c.Expr[Order[A]]): c.Expr[Ptr[T, C]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(first, ptr, maxPtr, vp, a, maxA) = util.names("first", "ptr", "maxPtr", "vp", "a", "maxA")
    val tree = q"""
{
  var $first: Boolean = true
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  var $maxPtr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC](-1L)
  var $maxA: $tagA = null.asInstanceOf[$tagA]
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    val $a: $tagA = $body($vp)
    if ($first || $orderA.gt($a, $maxA)) {
      $maxA = $a
      $maxPtr = $ptr
      $first = false
    }
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $maxPtr
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Ptr[T, C]](tree)
  }

  def minBy[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => A])(orderA: c.Expr[Order[A]]): c.Expr[Ptr[T, C]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(first, ptr, minPtr, vp, a, minA) = util.names("first", "ptr", "minPtr", "vp", "a", "minA")
    val tree = q"""
{
  var $first: Boolean = true
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  var $minPtr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC](-1L)
  var $minA: $tagA = null.asInstanceOf[$tagA]
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    val $a: $tagA = $body($vp)
    if ($first || $orderA.lt($a, $minA)) {
      $minA = $a
      $minPtr = $ptr
      $first = false
    }
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $minPtr
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Ptr[T, C]](tree)
  }

  def sumBy[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => A])(am: c.Expr[AdditiveMonoid[A]]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT, $tagC] = new Ptr[$tagT, $tagC]($lhs.raw)
  var $res: $tagA = $am.zero
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    $res = $am.plus($res, $body($vp))
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def productBy[T <: Pointable#Tag:c.WeakTypeTag, C <: Nextable:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(body: c.Expr[VPtr[T, C] => A])(mm: c.Expr[MultiplicativeMonoid[A]]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val container = extractPath[T](c)
    val tagT = implicitly[c.WeakTypeTag[T]]
    val tagC = implicitly[c.WeakTypeTag[C]]
    val tagA = implicitly[c.WeakTypeTag[A]]
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT, $tagC] =  new Ptr[$tagT, $tagC]($lhs.raw)
  var $res: $tagA = $mm.one
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT, $tagC] = new VPtr[$tagT, $tagC]($ptr.raw)
    $res = $mm.times($res, $body($vp))
    $ptr = new Ptr[$tagT, $tagC]($container.ptrNext(new VPtr[$container.Tag, $container.Cap]($vp.raw)).raw)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

}

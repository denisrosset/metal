package metal

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._

import MacroUtils._

object PtrMacros {

  def next[T <: Pointable#Tag](c: Context)(implicit tagT: c.WeakTypeTag[T]): c.Expr[Ptr[T]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    c.Expr[Ptr[T]](q"new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($lhs.v)).v)")
  }

  def remove[T <: Pointable#Tag](c: Context)(implicit tagT: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    c.Expr[Unit](q"$container.ptrRemove(new VPtr[$container.Tag]($lhs.v))")
  }

  def removeAndAdvance[T <: Pointable#Tag](c: Context)(implicit tagT: c.WeakTypeTag[T]): c.Expr[Ptr[T]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    c.Expr[Ptr[T]](q"new Ptr[$tagT]($container.ptrRemoveAndAdvance(new VPtr[$container.Tag]($lhs.v)).v)")
  }

  def key[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Keys](c)
    c.Expr[A](q"$container.ptrKey[$aType](new VPtr[$container.Tag]($lhs.v))")
  }

  def keyOrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Keys](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")

    c.Expr[A](q"""
{
  val $ptr: Ptr[$container.Tag] = new Ptr[$container.Tag]($lhs.v)
  if ($ptr.isNull) 
    $orElse 
  else 
    $container.ptrKey[$aType](new VPtr[$container.Tag]($ptr.v))
}
""")
  }

  def value[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values](c)
    c.Expr[A](q"$container.ptrValue[$aType](new VPtr[$container.Tag]($lhs.v))")
  }

  def valueOrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[A](q"""
{
  val $ptr: Ptr[$container.Tag] = new Ptr[$container.Tag]($lhs.v)
  if ($ptr.isNull) 
    $orElse 
  else 
    $container.ptrValue[$aType](new VPtr[$container.Tag]($ptr.v))
}
""")
  }

  def value1[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values1](c)
    c.Expr[A](q"$container.ptrValue1[$aType](new VPtr[$container.Tag]($lhs.v))")
  }

  def value1OrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values1](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[A](q"""
{
  val $ptr: Ptr[$container.Tag] = new Ptr[$container.Tag]($lhs.v)
  if ($ptr.isNull) 
    $orElse 
  else 
    $container.ptrValue1[$aType](new VPtr[$container.Tag]($ptr.v))
}
""")
  }

  def value2[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values2](c)
    c.Expr[A](q"$container.ptrValue2[$aType](new VPtr[$container.Tag]($lhs.v))")
  }

  def value2OrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values2](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[A](q"""
{
  val $ptr: Ptr[$container.Tag] = new Ptr[$container.Tag]($lhs.v)
  if ($ptr.isNull) 
    $orElse 
  else 
    $container.ptrValue2[$aType](new VPtr[$container.Tag]($ptr.v))
}
""")
  }

  def update[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Updatable](c)
    c.Expr[Unit](q"$container.ptrUpdate[$aType](new VPtr[$container.Tag]($lhs.v), $newValue)")
  }

  def update1[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Updatable1](c)
    c.Expr[Unit](q"$container.ptrUpdate1[$aType](new VPtr[$container.Tag]($lhs.v), $newValue)")
  }

  def update2[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Updatable2](c)
    c.Expr[Unit](q"$container.ptrUpdate2[$aType](new VPtr[$container.Tag]($lhs.v), $newValue)")
  }

  def foreach[T <: Pointable#Tag](c: Context)(body: c.Expr[VPtr[T] => Unit])(implicit tagT: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp) = util.names("ptr", "vp")
    val tree = q"""
{
  var $ptr: Ptr[$tagT] = $lhs
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    $body($vp)
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
}
""" // all the casts are needed
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def count[T <: Pointable#Tag](c: Context)(body: c.Expr[VPtr[T] => Boolean])(implicit tagT: c.WeakTypeTag[T]): c.Expr[Int] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, cnt) = util.names("ptr", "vp", "cnt")
    val tree = q"""
{
  var $ptr: Ptr[$tagT] = $lhs
  var $cnt: Int = 0
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    if ($body($vp)) $cnt += 1
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $cnt
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Int](tree)
  }

  def forall[T <: Pointable#Tag](c: Context)(body: c.Expr[VPtr[T] => Boolean])(implicit tagT: c.WeakTypeTag[T]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT] = $lhs
  var $res: Boolean = true
  while ($res && $ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    $res = $body($vp)
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def exists[T <: Pointable#Tag](c: Context)(body: c.Expr[VPtr[T] => Boolean])(implicit tagT: c.WeakTypeTag[T]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT] = $lhs
  var $res: Boolean = false
  while (!$res && $ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    $res = $body($vp)
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def foldLeft[T <: Pointable#Tag, A](c: Context)(z: c.Expr[A])(body: c.Expr[(A, VPtr[T]) => A])(implicit tagT: c.WeakTypeTag[T], tagA: c.WeakTypeTag[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT] = $lhs
  var $res: $tagA = $z
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    $res = $body($res, $vp)
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def maxBy[T <: Pointable#Tag, A](c: Context)(body: c.Expr[VPtr[T] => A])(orderA: c.Expr[Order[A]])(implicit tagT: c.WeakTypeTag[T], tagA: c.WeakTypeTag[A]): c.Expr[Ptr[T]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(first, ptr, maxPtr, vp, a, maxA) = util.names("first", "ptr", "maxPtr", "vp", "a", "maxA")
    val tree = q"""
{
  var $first: Boolean = true
  var $ptr: Ptr[$tagT] = $lhs
  var $maxPtr: Ptr[$tagT] = new Ptr[$tagT](-1L)
  var $maxA: $tagA = null.asInstanceOf[$tagA]
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    val $a: $tagA = $body($vp)
    if ($first || $orderA.gt($a, $maxA)) {
      $maxA = $a
      $maxPtr = $ptr
      $first = false
    }
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $maxPtr
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Ptr[T]](tree)
  }

  def minBy[T <: Pointable#Tag, A](c: Context)(body: c.Expr[VPtr[T] => A])(orderA: c.Expr[Order[A]])(implicit tagT: c.WeakTypeTag[T], tagA: c.WeakTypeTag[A]): c.Expr[Ptr[T]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(first, ptr, minPtr, vp, a, minA) = util.names("first", "ptr", "minPtr", "vp", "a", "minA")
    val tree = q"""
{
  var $first: Boolean = true
  var $ptr: Ptr[$tagT] = $lhs
  var $minPtr: Ptr[$tagT] = new Ptr[$tagT](-1L)
  var $minA: $tagA = null.asInstanceOf[$tagA]
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    val $a: $tagA = $body($vp)
    if ($first || $orderA.lt($a, $minA)) {
      $minA = $a
      $minPtr = $ptr
      $first = false
    }
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $minPtr
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Ptr[T]](tree)
  }

  def sumBy[T <: Pointable#Tag, A](c: Context)(body: c.Expr[VPtr[T] => A])(am: c.Expr[AdditiveMonoid[A]])(implicit tagT: c.WeakTypeTag[T], tagA: c.WeakTypeTag[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT] = $lhs
  var $res: $tagA = $am.zero
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    $res = $am.plus($res, $body($vp))
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def productBy[T <: Pointable#Tag, A](c: Context)(body: c.Expr[VPtr[T] => A])(mm: c.Expr[MultiplicativeMonoid[A]])(implicit tagT: c.WeakTypeTag[T], tagA: c.WeakTypeTag[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr, vp, res) = util.names("ptr", "vp", "res")
    val tree = q"""
{
  var $ptr: Ptr[$tagT] = $lhs
  var $res: $tagA = $mm.one
  while ($ptr.nonNull) {
    val $vp: VPtr[$tagT] = new VPtr[$tagT]($ptr.v)
    $res = $mm.times($res, $body($vp))
    $ptr = new Ptr[$tagT]($container.ptrNext(new VPtr[$container.Tag]($vp.v)).v)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

}


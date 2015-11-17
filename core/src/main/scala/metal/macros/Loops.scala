package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._
import spire.util.Opt

import MacroUtils._

object Loops {

  def foreach(c: Context)(call: Call[c.type], body: c.Tree): c.Expr[Unit] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp) = util.names("$cc", "$ptr", "$vp")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
while ($ptr.nonNull) {
  val $vp: VPtr[$cc.Tag, $cc.Cap] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  $bc
  $ptr = $cc.ptrNext($vp)
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def count(c: Context)(call: Call[c.type], body: c.Tree): c.Expr[Long] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("$cc", "$ptr", "$vp", "$res")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
var $res: Long = 0L
while ($ptr.nonNull) {
  val $vp: VPtr[$cc.Tag, $cc.Cap] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  if ($bc) $res += 1
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[Long](tree)
  }

  def exists(c: Context)(call: Call[c.type], body: c.Tree): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("$cc", "$ptr", "$vp", "$res")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
var $res: Boolean = false
while ($ptr.nonNull && !$res) {
  val $vp: VPtr[$cc.Tag] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  $res = $bc
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def forall(c: Context)(call: Call[c.type], body: c.Tree): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("$cc", "$ptr", "$vp", "$res")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
var $res: Boolean = true
while ($ptr.nonNull && $res) {
  val $vp: VPtr[$cc.Tag] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  $res = $bc
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def foldLeft[A](c: Context)(call: Call[c.type], body: c.Tree, initialValue: c.Expr[A])(implicit tagA: c.WeakTypeTag[A]): c.Expr[A] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("$cc", "$ptr", "$vp", "$res")
    val bc = call.withValue(util, lhs, cc, vp, body, res)
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
var $res: $tagA = $initialValue
while ($ptr.nonNull) {
  val $vp: VPtr[$cc.Tag, $cc.Cap] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  $res = $bc
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def minE[E:c.WeakTypeTag](c: Context)(order: c.Expr[Order[E]]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE = implicitly[c.WeakTypeTag[E]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, el, res) = util.names("$cc", "$ptr", "$vp", "$el", "$res")
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
if ($ptr.isNull) throw new UnsupportedOperationException("empty.min")
var $vp: VPtr[$cc.Tag, $cc.Cap] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
var $res: $tagE = $cc.ptrElement[$tagE]($vp)
$ptr = $cc.ptrNext($vp)
while ($ptr.nonNull) {
  $vp = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  val $el = $cc.ptrElement[$tagE]($vp)
  if ($order.lt($el, $res)) $res = $el
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def maxE[E:c.WeakTypeTag](c: Context)(order: c.Expr[Order[E]]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE = implicitly[c.WeakTypeTag[E]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, el, res) = util.names("$cc", "$ptr", "$vp", "$el", "$res")
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
if ($ptr.isNull) throw new UnsupportedOperationException("empty.min")
var $vp: VPtr[$cc.Tag, $cc.Cap] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
var $res: $tagE = $cc.ptrElement[$tagE]($vp)
$ptr = $cc.ptrNext($vp)
while ($ptr.nonNull) {
  $vp = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  val $el = $cc.ptrElement[$tagE]($vp)
  if ($order.gt($el, $res)) $res = $el
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def sumE[E:c.WeakTypeTag](c: Context)(am: c.Expr[AdditiveMonoid[E]]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE = implicitly[c.WeakTypeTag[E]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("$cc", "$ptr", "$vp", "$res")
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
var $res: $tagE = $am.zero
while ($ptr.nonNull) {
  val $vp: VPtr[$cc.Tag, $cc.Cap] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  $res = $am.plus($res, $cc.ptrElement[$tagE]($vp))
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def productE[E:c.WeakTypeTag](c: Context)(mm: c.Expr[MultiplicativeMonoid[E]]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE = implicitly[c.WeakTypeTag[E]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val tree = q"""
val $cc = $lhs
var $ptr: Ptr[$cc.Tag, $cc.Cap]  = $cc.ptr
var $res: $tagE = $mm.one
while ($ptr.nonNull) {
  val $vp: VPtr[$cc.Tag, $cc.Cap] = new VPtr[$cc.Tag, $cc.Cap]($ptr.raw)
  $res = $mm.times($res, $cc.ptrElement[$tagE]($vp))
  $ptr = $cc.ptrNext($vp)
}
$res
   """
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def foreachE[E:c.WeakTypeTag](c: Context)(body: c.Expr[E => Unit]): c.Expr[Unit] = foreach(c)(CallElements[c.type, E](c), body.tree)

  def foreachK[K:c.WeakTypeTag](c: Context)(body: c.Expr[K => Unit]): c.Expr[Unit] = foreach(c)(CallKeys[c.type, K](c), body.tree)

  def foreachV[V:c.WeakTypeTag](c: Context)(body: c.Expr[V => Unit]): c.Expr[Unit] = foreach(c)(CallValues[c.type, V](c), body.tree)

  def foreachKV[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V) => Unit]): c.Expr[Unit] = foreach(c)(CallKeysValues[c.type, K, V](c), body.tree)

  def foreachKV1V2[K:c.WeakTypeTag, V1:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V1, V2) => Unit]): c.Expr[Unit] = foreach(c)(CallKeysValues1Values2[c.type, K, V1, V2](c), body.tree)

  def countE[E:c.WeakTypeTag](c: Context)(body: c.Expr[E => Boolean]): c.Expr[Long] = count(c)(CallElements[c.type, E](c), body.tree)
  def countK[K:c.WeakTypeTag](c: Context)(body: c.Expr[K => Boolean]): c.Expr[Long] = count(c)(CallKeys[c.type, K](c), body.tree)
  def countV[V:c.WeakTypeTag](c: Context)(body: c.Expr[V => Boolean]): c.Expr[Long] = count(c)(CallValues[c.type, V](c), body.tree)
  def countKV[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V) => Boolean]): c.Expr[Long] = count(c)(CallKeysValues[c.type, K, V](c), body.tree)
  def countKV1V2[K:c.WeakTypeTag, V1:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V1, V2) => Boolean]): c.Expr[Long] = count(c)(CallKeysValues1Values2[c.type, K, V1, V2](c), body.tree)

  def existsE[E:c.WeakTypeTag](c: Context)(body: c.Expr[E => Boolean]): c.Expr[Boolean] = exists(c)(CallElements[c.type, E](c), body.tree)
  def existsK[K:c.WeakTypeTag](c: Context)(body: c.Expr[K => Boolean]): c.Expr[Boolean] = exists(c)(CallKeys[c.type, K](c), body.tree)
  def existsV[V:c.WeakTypeTag](c: Context)(body: c.Expr[V => Boolean]): c.Expr[Boolean] = exists(c)(CallValues[c.type, V](c), body.tree)
  def existsKV[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V) => Boolean]): c.Expr[Boolean] = exists(c)(CallKeysValues[c.type, K, V](c), body.tree)
  def existsKV1V2[K:c.WeakTypeTag, V1:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V1, V2) => Boolean]): c.Expr[Boolean] = exists(c)(CallKeysValues1Values2[c.type, K, V1, V2](c), body.tree)

  def forallE[E:c.WeakTypeTag](c: Context)(body: c.Expr[E => Boolean]): c.Expr[Boolean] = forall(c)(CallElements[c.type, E](c), body.tree)
  def forallK[K:c.WeakTypeTag](c: Context)(body: c.Expr[K => Boolean]): c.Expr[Boolean] = forall(c)(CallKeys[c.type, K](c), body.tree)
  def forallV[V:c.WeakTypeTag](c: Context)(body: c.Expr[V => Boolean]): c.Expr[Boolean] = forall(c)(CallValues[c.type, V](c), body.tree)
  def forallKV[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V) => Boolean]): c.Expr[Boolean] = forall(c)(CallKeysValues[c.type, K, V](c), body.tree)
  def forallKV1V2[K:c.WeakTypeTag, V1:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(body: c.Expr[(K, V1, V2) => Boolean]): c.Expr[Boolean] = forall(c)(CallKeysValues1Values2[c.type, K, V1, V2](c), body.tree)

  def foldLeftE[A:c.WeakTypeTag, E:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, E) => A]): c.Expr[A] = foldLeft(c)(CallElements[c.type, E](c), body.tree, initialValue)
  def foldLeftK[A:c.WeakTypeTag, K:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, K) => A]): c.Expr[A] = foldLeft(c)(CallKeys[c.type, K](c), body.tree, initialValue)
  def foldLeftV[A:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, V) => A]): c.Expr[A] = foldLeft(c)(CallValues[c.type, V](c), body.tree, initialValue)
  def foldLeftKV[A:c.WeakTypeTag, K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, K, V) => A]): c.Expr[A] = foldLeft(c)(CallKeysValues[c.type, K, V](c), body.tree, initialValue)
  def foldLeftKV1V2[A:c.WeakTypeTag, K:c.WeakTypeTag, V1:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, K, V1, V2) => A]): c.Expr[A] = foldLeft(c)(CallKeysValues1Values2[c.type, K, V1, V2](c), body.tree, initialValue)

}

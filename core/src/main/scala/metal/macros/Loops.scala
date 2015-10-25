package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._
import spire.util.Opt

import MacroUtils._

object Loops {

  def foreach(c: Context)(call: Call, body: c.Tree): c.Expr[Unit] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp) = util.names("cc", "ptr", "vp")
    val bc = call(c)(util, lhs, cc, vp, body)
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    $bc
    $ptr = $cc.ptrNext($vp)
  }
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def count(c: Context)(call: Call, body: c.Tree): c.Expr[Long] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call(c)(util, lhs, cc, vp, body)
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  var $res: Long = 0L
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    if ($bc) $res += 1
    $ptr = $cc.ptrNext($vp)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Long](tree)
  }

  def exists(c: Context)(call: Call, body: c.Tree): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call(c)(util, lhs, cc, vp, body)
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  var $res: Boolean = false
  while ($ptr.nonNull && !$res) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    $res = $bc
    $ptr = $cc.ptrNext($vp)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def forall(c: Context)(call: Call, body: c.Tree): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call(c)(util, lhs, cc, vp, body)
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  var $res: Boolean = true
  while ($ptr.nonNull && $res) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    $res = $bc
    $ptr = $cc.ptrNext($vp)
  }
  $res
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Boolean](tree)
  }

  def foldLeft[A](c: Context)(call: Call, body: c.Tree, initialValue: c.Expr[A])(implicit tagA: c.WeakTypeTag[A]): c.Expr[A] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call.withValue(c)(util, lhs, cc, vp, body, res)
    val tree = q"""
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  var $res: $tagA = $initialValue
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    $res = $bc
    $ptr = $cc.ptrNext($vp)
  }
  $res
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def min[E](c: Context)(order: c.Expr[Order[E]])(implicit tagE: c.WeakTypeTag[E]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, el, res) = util.names("cc", "ptr", "vp", "el", "res")
    val tree = q"""
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  if ($ptr.isNull) throw new UnsupportedOperationException("empty.min")
  var $res: $tagE = $cc.ptrElement[$tagE]($ptr.get)
  $ptr = $cc.ptrNext($ptr.get)
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    val $el = $cc.ptrElement[$tagE]($vp)
    if ($order.lt($el, $res)) $res = $el
    $ptr = $cc.ptrNext($vp)
  }
  $res
"""
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def max[E](c: Context)(order: c.Expr[Order[E]])(implicit tagE: c.WeakTypeTag[E]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, el, res) = util.names("cc", "ptr", "vp", "el", "res")
    val tree = q"""
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  if ($ptr.isNull) throw new UnsupportedOperationException("empty.min")
  var $res: $tagE = $cc.ptrElement[$tagE]($ptr.get)
  $ptr = $cc.ptrNext($ptr.get)
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    val $el = $cc.ptrElement[$tagE]($vp)
    if ($order.gt($el, $res)) $res = $el
    $ptr = $cc.ptrNext($vp)
  }
  $res
"""
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def sum[E](c: Context)(am: c.Expr[AdditiveMonoid[E]])(implicit tagE: c.WeakTypeTag[E]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val tree = q"""
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  var $res: $tagE = $am.zero
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    $res = $am.plus($res, $cc.ptrElement[$tagE]($vp))
    $ptr = $cc.ptrNext($vp)
  }
  $res
"""
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def product[E](c: Context)(mm: c.Expr[MultiplicativeMonoid[E]])(implicit tagE: c.WeakTypeTag[E]): c.Expr[E] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val tree = q"""
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptr
  var $res: $tagE = $mm.one
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    $res = $mm.times($res, $cc.ptrElement[$tagE]($vp))
    $ptr = $cc.ptrNext($vp)
  }
  $res
"""
    new InlineUtil[c.type](c).inlineAndReset[E](tree)
  }

  def foreachE[E](c: Context)(body: c.Expr[E => Unit]): c.Expr[Unit] = foreach(c)(CallE, body.tree)
  def foreachK[K](c: Context)(body: c.Expr[K => Unit]): c.Expr[Unit] = foreach(c)(CallK, body.tree)
  def foreachV[V](c: Context)(body: c.Expr[V => Unit]): c.Expr[Unit] = foreach(c)(CallV, body.tree)
  def foreachKV[K, V](c: Context)(body: c.Expr[(K, V) => Unit]): c.Expr[Unit] = foreach(c)(CallKV, body.tree)
  def foreachKV1V2[K, V1, V2](c: Context)(body: c.Expr[(K, V1, V2) => Unit]): c.Expr[Unit] = foreach(c)(CallKV1V2, body.tree)

  def countE[E](c: Context)(body: c.Expr[E => Boolean]): c.Expr[Long] = count(c)(CallE, body.tree)
  def countK[K](c: Context)(body: c.Expr[K => Boolean]): c.Expr[Long] = count(c)(CallK, body.tree)
  def countV[V](c: Context)(body: c.Expr[V => Boolean]): c.Expr[Long] = count(c)(CallV, body.tree)
  def countKV[K, V](c: Context)(body: c.Expr[(K, V) => Boolean]): c.Expr[Long] = count(c)(CallKV, body.tree)
  def countKV1V2[K, V1, V2](c: Context)(body: c.Expr[(K, V1, V2) => Boolean]): c.Expr[Long] = count(c)(CallKV1V2, body.tree)

  def existsE[E](c: Context)(body: c.Expr[E => Boolean]): c.Expr[Boolean] = exists(c)(CallE, body.tree)
  def existsK[K](c: Context)(body: c.Expr[K => Boolean]): c.Expr[Boolean] = exists(c)(CallK, body.tree)
  def existsV[V](c: Context)(body: c.Expr[V => Boolean]): c.Expr[Boolean] = exists(c)(CallV, body.tree)
  def existsKV[K, V](c: Context)(body: c.Expr[(K, V) => Boolean]): c.Expr[Boolean] = exists(c)(CallKV, body.tree)
  def existsKV1V2[K, V1, V2](c: Context)(body: c.Expr[(K, V1, V2) => Boolean]): c.Expr[Boolean] = exists(c)(CallKV1V2, body.tree)

  def forallE[E](c: Context)(body: c.Expr[E => Boolean]): c.Expr[Boolean] = forall(c)(CallE, body.tree)
  def forallK[K](c: Context)(body: c.Expr[K => Boolean]): c.Expr[Boolean] = forall(c)(CallK, body.tree)
  def forallV[V](c: Context)(body: c.Expr[V => Boolean]): c.Expr[Boolean] = forall(c)(CallV, body.tree)
  def forallKV[K, V](c: Context)(body: c.Expr[(K, V) => Boolean]): c.Expr[Boolean] = forall(c)(CallKV, body.tree)
  def forallKV1V2[K, V1, V2](c: Context)(body: c.Expr[(K, V1, V2) => Boolean]): c.Expr[Boolean] = forall(c)(CallKV1V2, body.tree)

  def foldLeftE[A:c.WeakTypeTag, E](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, E) => A]): c.Expr[A] = foldLeft(c)(CallE, body.tree, initialValue)
  def foldLeftK[A:c.WeakTypeTag, K](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, K) => A]): c.Expr[A] = foldLeft(c)(CallK, body.tree, initialValue)
  def foldLeftV[A:c.WeakTypeTag, V](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, V) => A]): c.Expr[A] = foldLeft(c)(CallV, body.tree, initialValue)
  def foldLeftKV[A:c.WeakTypeTag, K, V](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, K, V) => A]): c.Expr[A] = foldLeft(c)(CallKV, body.tree, initialValue)
  def foldLeftKV1V2[A:c.WeakTypeTag, K, V1, V2](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, K, V1, V2) => A]): c.Expr[A] = foldLeft(c)(CallKV1V2, body.tree, initialValue)

  def minE[E:c.WeakTypeTag](c: Context)(order: c.Expr[Order[E]]): c.Expr[E] = min[E](c)(order)

  def maxE[E:c.WeakTypeTag](c: Context)(order: c.Expr[Order[E]]): c.Expr[E] = max[E](c)(order)

  def sumE[E:c.WeakTypeTag](c: Context)(am: c.Expr[AdditiveMonoid[E]]): c.Expr[E] = sum[E](c)(am)

  def productE[E:c.WeakTypeTag](c: Context)(mm: c.Expr[MultiplicativeMonoid[E]]): c.Expr[E] = product[E](c)(mm)

}

package metal
package macros

import spire.macros.compat.Context
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._

import MacroUtils._

object Loops {

  def foreach(c: Context)(call: Call[c.type], body: c.Tree): c.Expr[Unit] = {
    import c.universe._
    val lhs = findLhs(c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp) = util.names("cc", "ptr", "vp")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
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
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
var $res: Long = 0L
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
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
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
var $res: Boolean = false
while ($ptr.nonNull && !$res) {
  val $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
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
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call(util, lhs, cc, vp, body)
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
var $res: Boolean = true
while ($ptr.nonNull && $res) {
  val $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
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
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val bc = call.withValue(util, lhs, cc, vp, body, res)
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
var $res: $tagA = $initialValue
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
  $res = $bc
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[A](tree)
  }

  def min1[E1:c.WeakTypeTag](c: Context)(order: c.Expr[Order[E1]]): c.Expr[E1] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE1 = implicitly[c.WeakTypeTag[E1]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, el, res) = util.names("cc", "ptr", "vp", "el", "res")
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
if ($ptr.isNull) throw new UnsupportedOperationException("empty.min")
var $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
var $res: $tagE1 = $cc.ptrElement1[$tagE1]($vp)
$ptr = $cc.ptrNext($vp)
while ($ptr.nonNull) {
  $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
  val $el = $cc.ptrElement1[$tagE1]($vp)
  if ($order.lt($el, $res)) $res = $el
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[E1](tree)
  }

  def max1[E1:c.WeakTypeTag](c: Context)(order: c.Expr[Order[E1]]): c.Expr[E1] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE1 = implicitly[c.WeakTypeTag[E1]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, el, res) = util.names("cc", "ptr", "vp", "el", "res")
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
if ($ptr.isNull) throw new UnsupportedOperationException("empty.min")
var $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
var $res: $tagE1 = $cc.ptrElement1[$tagE1]($vp)
$ptr = $cc.ptrNext($vp)
while ($ptr.nonNull) {
  $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
  val $el = $cc.ptrElement1[$tagE1]($vp)
  if ($order.gt($el, $res)) $res = $el
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[E1](tree)
  }

  def sum1[E1:c.WeakTypeTag](c: Context)(am: c.Expr[AdditiveMonoid[E1]]): c.Expr[E1] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE1 = implicitly[c.WeakTypeTag[E1]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
var $res: $tagE1 = $am.zero
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
  $res = $am.plus($res, $cc.ptrElement1[$tagE1]($vp))
  $ptr = $cc.ptrNext($vp)
}
$res
"""
    new InlineUtil[c.type](c).inlineAndReset[E1](tree)
  }

  def product1[E1:c.WeakTypeTag](c: Context)(mm: c.Expr[MultiplicativeMonoid[E1]]): c.Expr[E1] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagE1 = implicitly[c.WeakTypeTag[E1]]
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, res) = util.names("cc", "ptr", "vp", "res")
    val tree = q"""
val $cc = $lhs
var $ptr = $cc.ptr
var $res: $tagE1 = $mm.one
while ($ptr.nonNull) {
  val $vp = _root_.metal.VPtr[$cc.type]($ptr.raw)
  $res = $mm.times($res, $cc.ptrElement1[$tagE1]($vp))
  $ptr = $cc.ptrNext($vp)
}
$res
   """
    new InlineUtil[c.type](c).inlineAndReset[E1](tree)
  }

  def foreach1[E1:c.WeakTypeTag](c: Context)(body: c.Expr[E1 => Unit]): c.Expr[Unit] = foreach(c)(CallElements1[c.type, E1](c), body.tree)

  def foreach2[E1:c.WeakTypeTag, E2:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2) => Unit]): c.Expr[Unit] = foreach(c)(CallElements2[c.type, E1, E2](c), body.tree)

  def foreach3[E1:c.WeakTypeTag, E2:c.WeakTypeTag, E3:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2, E3) => Unit]): c.Expr[Unit] = foreach(c)(CallElements3[c.type, E1, E2, E3](c), body.tree)

  def count1[E1:c.WeakTypeTag](c: Context)(body: c.Expr[E1 => Boolean]): c.Expr[Long] = count(c)(CallElements1[c.type, E1](c), body.tree)

  def count2[E1:c.WeakTypeTag, E2:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2) => Boolean]): c.Expr[Long] = count(c)(CallElements2[c.type, E1, E2](c), body.tree)

  def count3[E1:c.WeakTypeTag, E2:c.WeakTypeTag, E3:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2, E3) => Boolean]): c.Expr[Long] = count(c)(CallElements3[c.type, E1, E2, E3](c), body.tree)

  def exists1[E1:c.WeakTypeTag](c: Context)(body: c.Expr[E1 => Boolean]): c.Expr[Boolean] = exists(c)(CallElements1[c.type, E1](c), body.tree)

  def exists2[E1:c.WeakTypeTag, E2:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2) => Boolean]): c.Expr[Boolean] = exists(c)(CallElements2[c.type, E1, E2](c), body.tree)

  def exists3[E1:c.WeakTypeTag, E2:c.WeakTypeTag, E3:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2, E3) => Boolean]): c.Expr[Boolean] = exists(c)(CallElements3[c.type, E1, E2, E3](c), body.tree)

  def forall1[E1:c.WeakTypeTag](c: Context)(body: c.Expr[E1 => Boolean]): c.Expr[Boolean] = forall(c)(CallElements1[c.type, E1](c), body.tree)

  def forall2[E1:c.WeakTypeTag, E2:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2) => Boolean]): c.Expr[Boolean] = forall(c)(CallElements2[c.type, E1, E2](c), body.tree)

  def forall3[E1:c.WeakTypeTag, E2:c.WeakTypeTag, E3:c.WeakTypeTag](c: Context)(body: c.Expr[(E1, E2, E3) => Boolean]): c.Expr[Boolean] = forall(c)(CallElements3[c.type, E1, E2, E3](c), body.tree)

  def foldLeft1[A:c.WeakTypeTag, E1:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, E1) => A]): c.Expr[A] = foldLeft(c)(CallElements1[c.type, E1](c), body.tree, initialValue)

  def foldLeft2[A:c.WeakTypeTag, E1:c.WeakTypeTag, E2:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, E1, E2) => A]): c.Expr[A] = foldLeft(c)(CallElements2[c.type, E1, E2](c), body.tree, initialValue)

  def foldLeft3[A:c.WeakTypeTag, E1:c.WeakTypeTag, E2:c.WeakTypeTag, E3:c.WeakTypeTag](c: Context)(initialValue: c.Expr[A])(body: c.Expr[(A, E1, E2, E3) => A]): c.Expr[A] = foldLeft(c)(CallElements3[c.type, E1, E2, E3](c), body.tree, initialValue)

}

package metal

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}
import spire.util.Opt

import MacroUtils._

object LoopMacros {

  def foreachK[K](c: Context)(body: c.Expr[K => Unit]): c.Expr[Unit] = {
    import c.universe._
    val (lhs, kType) = findLhsType[Keys](c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, key) = util.names("cc", "ptr", "vp", "key")
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptrStart
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    val $key: $kType = $cc.ptrKey($vp)
    $body($key)
    $ptr = $cc.ptrNext($vp)
  }
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def foreachV[V](c: Context)(body: c.Expr[V => Unit]): c.Expr[Unit] = {
    import c.universe._
    val (lhs, vType) = findLhsType[Values](c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, value) = util.names("cc", "ptr", "vp", "value")
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptrStart
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    val $value: $vType = $cc.ptrValue($vp)
    $body($value)
    $ptr = $cc.ptrNext($vp)
  }
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def foreachKV[K, V](c: Context)(body: c.Expr[(K, V) => Unit]): c.Expr[Unit] = {
    import c.universe._
    val (lhs, kType, vType) = findLhsTypeType[Keys, Values](c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, key, value) = util.names("cc", "ptr", "vp", "key", "value")
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptrStart
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    val $key: $kType = $cc.ptrKey($vp)
    val $value: $vType = $cc.ptrValue($vp)
    $body($key, $value)
    $ptr = $cc.ptrNext($vp)
  }
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

  def foreachKV1V2[K, V1, V2](c: Context)(body: c.Expr[(K, V1, V2) => Unit]): c.Expr[Unit] = {
    import c.universe._
    val (lhs, kType, v1Type, v2Type) = findLhsTypeTypeType[Keys, Values1, Values2](c)
    val util = SyntaxUtil[c.type](c)
    val List(cc, ptr, vp, key, value1, value2) = util.names("cc", "ptr", "vp", "key", "value1", "value2")
    val tree = q"""
{
  val $cc = $lhs
  var $ptr: Ptr[$cc.Tag]  = $cc.ptrStart
  while ($ptr.nonNull) {
    val $vp: VPtr[$cc.Tag] = $ptr.get
    val $key: $kType = $cc.ptrKey($vp)
    val $value1: $v1Type = $cc.ptrValue1($vp)
    val $value2: $v2Type = $cc.ptrValue2($vp)
    $body($key, $value1, $value2)
    $ptr = $cc.ptrNext($vp)
  }
}
"""
    new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  }

}

/*

def foreach[U](p: K => U): Unit
def foreach[U](p: (K, V) => U): Unit
def foreach[U](p: (K, V1, V2) => U): Unit

def count(p: K => Boolean): Int
def count(p: (K, V) => Boolean): Int
def count(p: (K, V1, V2) => Boolean): Int

def exists(p: K => Boolean): Boolean
def exists(p: (K, V) => Boolean): Boolean
def exists(p: (K, V1, V2) => Boolean): Boolean

def forall(p: K => Boolean): Boolean
def forall(p: (K, V) => Boolean): Boolean
def forall(p: (K, V1, V2) => Boolean): Boolean

def foldLeft[B](z: B)(op: (B, K) => B): B
def foldLeft[B](z: B)(op: (B, K, V) => B): B
def foldLeft[B](z: B)(op: (B, K, V1, V2) => B): B

def foldRight[B](z: B)(op: (K, B) => B): B
def foldRight[B](z: B)(op: (K, V, B) => B): B
def foldRight[B](z: B)(op: (K, V1, V2, B) => B): B

def max(implicit o: Order[K]): K
def maxBy[A](f: K => A)(implicit o: Order[A]): K
def min(implicit o: Order[K]): K
def minBy[A](f: K => A)(implicit o: Order[A]): K
def product(ev: AdditiveMonoid[K]): K
def sum(ev: MultiplicativeMonoid[K]) : K

 */

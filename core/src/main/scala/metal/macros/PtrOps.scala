package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._

import MacroUtils._

object PtrOps {

  def element1OrElse[T:c.WeakTypeTag, E1:c.WeakTypeTag](c: Context)(orElse: c.Expr[E1]): c.Expr[E1] = {
    import c.universe._
    val lhs = c.prefix.tree
    val e1Type = implicitly[c.WeakTypeTag[E1]]
    val container = extractPath[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[E1](q"""
{
  val $ptr: Ptr[$container.Tag, $container.Cap] = new Ptr[$container.Tag, $container.Cap]($lhs.raw)
  if ($ptr.isNull) 
    $orElse
  else
    $container.ptrElement1[$e1Type](new VPtr[$container.Tag, $container.Cap]($ptr.raw))
}
""")
  }

  def keyOrElse[T:c.WeakTypeTag, K:c.WeakTypeTag](c: Context)(orElse: c.Expr[K]): c.Expr[K] = {
    import c.universe._
    val lhs = c.prefix.tree
    val kType = implicitly[c.WeakTypeTag[K]]
    val container = extractPath[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[K](q"""
{
  val $ptr: Ptr[$container.Tag, $container.Cap] = new Ptr[$container.Tag, $container.Cap]($lhs.raw)
  if ($ptr.isNull) 
    $orElse
  else
    $container.ptrKey[$kType](new VPtr[$container.Tag, $container.Cap]($ptr.raw))
}
""")
  }

  def valueOrElse[T:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(orElse: c.Expr[V]): c.Expr[V] = {
    import c.universe._
    val lhs = c.prefix.tree
    val vType = implicitly[c.WeakTypeTag[V]]
    val container = extractPath[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[V](q"""
{
  val $ptr: Ptr[$container.Tag, $container.Cap] = new Ptr[$container.Tag, $container.Cap]($lhs.raw)
  if ($ptr.isNull) 
    $orElse
  else
    $container.ptrValue[$vType](new VPtr[$container.Tag, $container.Cap]($ptr.raw))
}
""")
  }

  def value1OrElse[T:c.WeakTypeTag, V1:c.WeakTypeTag](c: Context)(orElse: c.Expr[V1]): c.Expr[V1] = {
    import c.universe._
    val lhs = c.prefix.tree
    val v1Type = implicitly[c.WeakTypeTag[V1]]
    val container = extractPath[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[V1](q"""
{
  val $ptr: Ptr[$container.Tag, $container.Cap] = new Ptr[$container.Tag, $container.Cap]($lhs.raw)
  if ($ptr.isNull) 
    $orElse
  else
    $container.ptrValue1[$v1Type](new VPtr[$container.Tag, $container.Cap]($ptr.raw))
}
""")
  }

  def value2OrElse[T:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(orElse: c.Expr[V2]): c.Expr[V2] = {
    import c.universe._
    val lhs = c.prefix.tree
    val v2Type = implicitly[c.WeakTypeTag[V2]]
    val container = extractPath[T](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[V2](q"""
{
  val $ptr: Ptr[$container.Tag, $container.Cap] = new Ptr[$container.Tag, $container.Cap]($lhs.raw)
  if ($ptr.isNull) 
    $orElse
  else
    $container.ptrValue2[$v2Type](new VPtr[$container.Tag, $container.Cap]($ptr.raw))
}
""")
  }

}

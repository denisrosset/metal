package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import MacroUtils._

object PtrOps {

  def element1OrElse[C <: Elements1[E1] with Singleton:c.WeakTypeTag, E1:c.WeakTypeTag](c: Context)(orElse: c.Expr[E1]): c.Expr[E1] = {
    import c.universe._
    val lhs = c.prefix.tree
    val e1Type = implicitly[c.WeakTypeTag[E1]]
    val container = extractSingleton[C](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[E1](q"""
val $ptr = _root_.metal.Ptr[$container.type]($lhs.raw)
if ($ptr.isNull) 
  $orElse
else
  $container.ptrElement1[$e1Type](_root_.metal.VPtr[$container.type]($ptr.raw))
""")
  }

  def element2OrElse[C <: Elements2[E2] with Singleton:c.WeakTypeTag, E2:c.WeakTypeTag](c: Context)(orElse: c.Expr[E2]): c.Expr[E2] = {
    import c.universe._
    val lhs = c.prefix.tree
    val e2Type = implicitly[c.WeakTypeTag[E2]]
    val container = extractSingleton[C](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[E2](q"""
val $ptr = _root_.metal.Ptr[$container.type]($lhs.raw)
if ($ptr.isNull) 
  $orElse
else
  $container.ptrElement2[$e2Type](_root_.metal.VPtr[$container.type]($ptr.raw))
""")
  }

  def element3OrElse[C <: Elements3[E3] with Singleton:c.WeakTypeTag, E3:c.WeakTypeTag](c: Context)(orElse: c.Expr[E3]): c.Expr[E3] = {
    import c.universe._
    val lhs = c.prefix.tree
    val e3Type = implicitly[c.WeakTypeTag[E3]]
    val container = extractSingleton[C](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[E3](q"""
val $ptr = _root_.metal.Ptr[$container.Tag, $container.Cap]($lhs.raw)
if ($ptr.isNull) 
  $orElse
else
  $container.ptrElement3[$e3Type](_root_.metal.VPtr[$container.type]($ptr.raw))
""")
  }

  def keyOrElse[C <: Keys[K] with Singleton:c.WeakTypeTag, K:c.WeakTypeTag](c: Context)(orElse: c.Expr[K]): c.Expr[K] = {
    import c.universe._
    val lhs = c.prefix.tree
    val kType = implicitly[c.WeakTypeTag[K]]
    val container = extractSingleton[C](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[K](q"""
val $ptr = _root_.metal.Ptr[$container.type]($lhs.raw)
if ($ptr.isNull) 
  $orElse
else
  $container.ptrKey[$kType](_root_.metal.VPtr[$container.type]($ptr.raw))
""")
  }

  def valueOrElse[C <: Values[V] with Singleton:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(orElse: c.Expr[V]): c.Expr[V] = {
    import c.universe._
    val lhs = c.prefix.tree
    val vType = implicitly[c.WeakTypeTag[V]]
    val container = extractSingleton[C](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[V](q"""
val $ptr = _root_.metal.Ptr[$container.type]($lhs.raw)
if ($ptr.isNull) 
  $orElse
else
  $container.ptrValue[$vType](_root_.metal.VPtr[$container.type]($ptr.raw))
""")
  }

  def value1OrElse[C <: Values1[V1] with Singleton:c.WeakTypeTag, V1:c.WeakTypeTag](c: Context)(orElse: c.Expr[V1]): c.Expr[V1] = {
    import c.universe._
    val lhs = c.prefix.tree
    val v1Type = implicitly[c.WeakTypeTag[V1]]
    val container = extractSingleton[C](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[V1](q"""
val $ptr = _root_.metal.Ptr[$container.type]($lhs.raw)
if ($ptr.isNull) 
  $orElse
else
  $container.ptrValue1[$v1Type](_root_.metal.VPtr[$container.type]($ptr.raw))
""")
  }

  def value2OrElse[C <: Values2[V2] with Singleton:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(orElse: c.Expr[V2]): c.Expr[V2] = {
    import c.universe._
    val lhs = c.prefix.tree
    val v2Type = implicitly[c.WeakTypeTag[V2]]
    val container = extractSingleton[C](c)
    val util = SyntaxUtil[c.type](c)
    val List(ptr) = util.names("ptr")
    c.Expr[V2](q"""
val $ptr = _root_.metal.Ptr[$container.type]($lhs.raw)
if ($ptr.isNull) 
  $orElse
else
  $container.ptrValue2[$v2Type](_root_.metal.VPtr[$container.type]($ptr.raw))
""")
  }

}

package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import spire.algebra._

import MacroUtils._

object PtrOps {

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

}

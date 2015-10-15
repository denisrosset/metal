package metal

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}

import MacroUtils._

object PtrMacros {

  def next[T <: Pointable#Tag](c: Context)(implicit tagT: c.WeakTypeTag[T]): c.Expr[Ptr[T]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    c.Expr[Ptr[T]](q"Ptr[$tagT]($container.ptrNext(VPtr[$container.Tag]($lhs.v)).v)")
  }

  def remove[T <: Pointable#Tag](c: Context)(implicit tagT: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    c.Expr[Unit](q"$container.ptrRemove(VPtr[$container.Tag]($lhs.v))")
  }

  def removeAndAdvance[T <: Pointable#Tag](c: Context)(implicit tagT: c.WeakTypeTag[T]): c.Expr[Ptr[T]] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, _) = extract[T](c)
    c.Expr[Ptr[T]](q"Ptr[$tagT]($container.ptrRemoveAndAdvance(VPtr[$container.Tag]($lhs.v)).v)")
  }

  def key[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Keys](c)
    c.Expr[A](q"$container.ptrKey[$aType](VPtr[$container.Tag]($lhs.v))")
  }

  def keyOrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Keys](c)
    c.Expr[A](q"""
if ($lhs.isNull) 
  $orElse 
else 
  $container.ptrKey[$aType](VPtr[$container.Tag]($lhs.v))
""")
  }

  def value[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values](c)
    c.Expr[A](q"$container.ptrValue[$aType](VPtr[$container.Tag]($lhs.v))")
  }

  def valueOrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values](c)
    c.Expr[A](q"""
if ($lhs.isNull) 
  $orElse 
else 
  $container.ptrValue[$aType](VPtr[$container.Tag]($lhs.v))
""")
  }

  def value1[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values1](c)
    c.Expr[A](q"$container.ptrValue1[$aType](VPtr[$container.Tag]($lhs.v))")
  }

  def value1OrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values1](c)
    c.Expr[A](q"""
if ($lhs.isNull) 
  $orElse 
else 
  $container.ptrValue1[$aType](VPtr[$container.Tag]($lhs.v))
""")
  }

  def value2[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values2](c)
    c.Expr[A](q"$container.ptrValue2[$aType](VPtr[$container.Tag]($lhs.v))")
  }

  def value2OrElse[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Values2](c)
    c.Expr[A](q"""
if ($lhs.isNull) 
  $orElse 
else 
  $container.ptrValue2[$aType](VPtr[$container.Tag]($lhs.v))
""")
  }

  def update[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Updatable](c)
    c.Expr[Unit](q"$container.ptrUpdate[$aType](VPtr[$container.Tag]($lhs.v), $newValue)")
  }

  def update1[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Updatable1](c)
    c.Expr[Unit](q"$container.ptrUpdate1[$aType](VPtr[$container.Tag]($lhs.v), $newValue)")
  }

  def update2[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Updatable2](c)
    c.Expr[Unit](q"$container.ptrUpdate2[$aType](VPtr[$container.Tag]($lhs.v), $newValue)")
  }

}

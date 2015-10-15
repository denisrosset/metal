package metal

import scala.reflect.macros.whitebox.Context

object PtrMacros {

  def extract[T](c: Context)(implicit tagT: c.WeakTypeTag[T]): (c.Symbol, c.Type) = {
    import c.universe._
    tagT.tpe match {
      case TypeRef(containerType@SingleType(_, container), _, Nil) => (container, containerType)
      case t => c.abort(c.enclosingPosition, "Cannot extract container value from path dependent type (type = %s)" format t)
    }
  }

  def extractTypeOf[T, TC[_]](c: Context)(implicit tagT: c.WeakTypeTag[T], tc: c.WeakTypeTag[TC[_]]): (c.Symbol, c.Type, c.Type) = {
    import c.universe._
    tagT.tpe match {
      case TypeRef(containerType@SingleType(_, container), _, Nil) =>
        val tcClass: ClassSymbol = tc.tpe.typeSymbol.asClass
        val tcTypeParam: Type = tcClass.typeParams(0).asType.toType
        val aType: Type = tcTypeParam.asSeenFrom(containerType, tcClass)
        (container, containerType, aType)
      case t => c.abort(c.enclosingPosition, "Cannot extract container value from path dependent type (type = %s)" format t)
    }
  }

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

  def value[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Keys](c)
    c.Expr[A](q"$container.ptrValue[$aType](VPtr[$container.Tag]($lhs.v))")
  }

  def update[T:c.WeakTypeTag, A:c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType, aType) = extractTypeOf[T, Keys](c)
    c.Expr[Unit](q"$container.ptrUpdate[$aType](VPtr[$container.Tag]($lhs.v), $newValue)")
  }

  /*
   def value1[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Values1](c)(containerType) match {
      case None => c.Expr[A](q"$container.ptrValue1(VPtr[$container.Tag]($lhs.v))")
      case Some(primitive) => c.Expr[A](q"$primitive.decode($container.ptrValue1P(VPtr[$container.Tag]($lhs.v)))")
    }
  }

  def value2[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Values2](c)(containerType) match {
      case None => c.Expr[A](q"$container.ptrValue2(VPtr[$container.Tag]($lhs.v))")
      case Some(primitive) => c.Expr[A](q"$primitive.decode($container.ptrValue2P(VPtr[$container.Tag]($lhs.v)))")
    }
  }

  def keyOrElse[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Keys](c)(containerType) match {
      case None => c.Expr[A](q"if ($lhs.isNull) $orElse else $container.ptrKey(VPtr[$container.Tag]($lhs.v))")
      case Some(primitive) => c.Expr[A](q"if ($lhs.isNull) $orElse else $primitive.decode($container.ptrKeyP(VPtr[$container.Tag]($lhs.v)))")
    }
  }

  def valueOrElse[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Values](c)(containerType) match {
      case None => c.Expr[A](q"if ($lhs.isNull) $orElse else $container.ptrValue(VPtr[$container.Tag]($lhs.v))")
      case Some(primitive) => c.Expr[A](q"if ($lhs.isNull) $orElse else $primitive.decode($container.ptrValueP(VPtr[$container.Tag]($lhs.v)))")
    }
  }

  def value1OrElse[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Values1](c)(containerType) match {
      case None => c.Expr[A](q"if ($lhs.isNull) $orElse else $container.ptrValue1(VPtr[$container.Tag]($lhs.v))")
      case Some(primitive) => c.Expr[A](q"if ($lhs.isNull) $orElse else $primitive.decode($container.ptrValue1P(VPtr[$container.Tag]($lhs.v)))")
    }
  }

  def value2OrElse[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)(orElse: c.Expr[A]): c.Expr[A] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Values2](c)(containerType) match {
      case None => c.Expr[A](q"if ($lhs.isNull) $orElse else $container.ptrValue2(VPtr[$container.Tag]($lhs.v))")
      case Some(primitive) => c.Expr[A](q"if ($lhs.isNull) $orElse else $primitive.decode($container.ptrValue2P(VPtr[$container.Tag]($lhs.v)))")
    }
  }

  def update1[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Values1](c)(containerType) match {
      case None => c.Expr[Unit](q"$container.ptrUpdate1(VPtr[$container.Tag]($lhs.v), $newValue)")
      case Some(primitive) => c.Expr[Unit](q"$container.ptrUpdate1P(VPtr[$container.Tag], $primitive.encode($newValue))")
    }
  }

  def update2[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context)(newValue: c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val lhs = c.prefix.tree
    val (container, containerType) = extract[T](c)
    primitive[T, Values2](c)(containerType) match {
      case None => c.Expr[Unit](q"$container.ptrUpdate2(VPtr[$container.Tag]($lhs.v), $newValue)")
      case Some(primitive) => c.Expr[Unit](q"$container.ptrUpdate2P(VPtr[$container.Tag], $primitive.encode($newValue))")
    }
  }*/

}

package net.alasc.ptrcoll

import scala.reflect.macros.whitebox.Context

object Macros {

  def next[T <: Pointable#Tag : c.WeakTypeTag](c: Context): c.Expr[Ptr[T]] = {
    import c.universe._
    val ptr = c.prefix.tree
    val tagT = implicitly[c.WeakTypeTag[T]]
    val TypeRef(SingleType(_, container), _, Nil) = tagT.tpe
    c.Expr[Ptr[T]](q"$container.ptrNext($ptr.asInstanceOf[VPtr[$container.Tag]]).asInstanceOf[Ptr[$tagT]]")
  }

  def remove[T <: Pointable#Tag : c.WeakTypeTag](c: Context): c.Expr[Unit] = {
    import c.universe._
    val ptr = c.prefix.tree
    val tagT = implicitly[c.WeakTypeTag[T]]
    val TypeRef(SingleType(_, container), _, Nil) = tagT.tpe
    c.Expr[Unit](q"$container.ptrRemove($ptr.asInstanceOf[VPtr[$container.Tag]])")
  }

  def removeAndAdvance[T <: Pointable#Tag : c.WeakTypeTag](c: Context): c.Expr[Ptr[T]] = {
    import c.universe._
    val ptr = c.prefix.tree
    val tagT = implicitly[c.WeakTypeTag[T]]
    val TypeRef(SingleType(_, container), _, Nil) = tagT.tpe
    c.Expr[Ptr[T]](q"$container.ptrRemoveAndAdvance($ptr.asInstanceOf[VPtr[$container.Tag]]).asInstanceOf[Ptr[$tagT]]")
  }

  /*    val ptkClass = typeOf[PointableKey[_]].typeSymbol.asClass
    val tType = ptkClass.typeParams(0).asType.toType
    val aType = tType.asSeenFrom(implicitly[c.WeakTypeTag[T]].tpe, ptkClass)*/

  def key[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val ptr = c.prefix.tree
    val tagT = implicitly[c.WeakTypeTag[T]]
    val TypeRef(SingleType(_, container), _, Nil) = tagT.tpe
    c.Expr[A](q"$container.ptrKey($ptr.asInstanceOf[VPtr[$container.Tag]])")
  }

  def value[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val ptr = c.prefix.tree
    val tagT = implicitly[c.WeakTypeTag[T]]
    val TypeRef(SingleType(_, container), _, Nil) = tagT.tpe
    c.Expr[A](q"$container.ptrValue($ptr.asInstanceOf[VPtr[$container.Tag]])")
  }

  def value1[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val ptr = c.prefix.tree
    val tagT = implicitly[c.WeakTypeTag[T]]
    val TypeRef(SingleType(_, container), _, Nil) = tagT.tpe
    c.Expr[A](q"$container.ptrValue1($ptr.asInstanceOf[VPtr[$container.Tag]])")
  }

  def value2[T : c.WeakTypeTag, A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    val ptr = c.prefix.tree
    val tagT = implicitly[c.WeakTypeTag[T]]
    val TypeRef(SingleType(_, container), _, Nil) = tagT.tpe
    c.Expr[A](q"$container.ptrValue2($ptr.asInstanceOf[VPtr[$container.Tag]])")
  }

}

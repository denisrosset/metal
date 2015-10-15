package metal

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}
import spire.util.Opt

object OpsMacros {

  def findLhs(c: Context): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) => lhs
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

  def findLhsType[TC[_]](c: Context)(implicit tc: c.WeakTypeTag[TC[_]]): (c.Tree, c.Type) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        val tcClass: ClassSymbol = tc.tpe.typeSymbol.asClass
        val tcTypeParam: Type = tcClass.typeParams(0).asType.toType
        val aType: Type = tcTypeParam.asSeenFrom(lhs.tpe, tcClass)
        (lhs, aType)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }


  def findLhsTypeType[TC1[_], TC2[_]](c: Context)(implicit tc1: c.WeakTypeTag[TC1[_]], tc2: c.WeakTypeTag[TC2[_]]): (c.Tree, c.Type, c.Type) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        val tc1Class: ClassSymbol = tc1.tpe.typeSymbol.asClass
        val tc1TypeParam: Type = tc1Class.typeParams(0).asType.toType
        val tc2Class: ClassSymbol = tc2.tpe.typeSymbol.asClass
        val tc2TypeParam: Type = tc2Class.typeParams(0).asType.toType
        val a1Type: Type = tc1TypeParam.asSeenFrom(lhs.tpe, tc1Class)
        val a2Type: Type = tc2TypeParam.asSeenFrom(lhs.tpe, tc2Class)
        (lhs, a1Type, a2Type)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

  def contains[K](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val (lhs, kType) = findLhsType[Keys](c)
    c.Expr[Boolean](q"$lhs.ptrFind[$kType]($key).nonNull")
  }

  def remove[K](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val (lhs, kType) = findLhsType[Keys](c)
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[Boolean](q"""
{
  val $lhsCache = $lhs
  $lhsCache.ptrFind[$kType]($key) match { 
    case VPtr(vp) => 
      $lhsCache.ptrRemove(vp)
      true
    case _ => 
      false 
  }
}""")
  }

  def -=[K, T](c: Context)(key: c.Expr[K]): c.Expr[T] = {
    import c.universe._
    val (lhs, kType) = findLhsType[Removable](c)
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[T](q"""
{
  val $lhsCache = $lhs
  $lhsCache.ptrFind[$kType]($key) match {
    case VPtr(vp) => 
      $lhsCache.ptrRemove(vp)
      $lhs
    case _ => 
      $lhs 
  }
}
""")
  }

  def +=[K, T](c: Context)(key: c.Expr[K]): c.Expr[T] = {
    import c.universe._
    val (lhs, kType) = findLhsType[AddKeys](c)
    c.Expr[T](q"$lhs.ptrAddKey[$kType]($key); $lhs")
  }

  def add[K](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val util = SyntaxUtil[c.type](c)
    val (lhs, kType) = findLhsType[AddKeys](c)
    val lhsCache = util.name("lhsCache")
    val keyCache = util.name("keyCache")
    val contained = util.name("contained")
    c.Expr[Boolean](q"""
{
  val $lhsCache = $lhs
  val $keyCache = $key
  val $contained = $lhsCache.ptrFind[$kType]($keyCache).nonNull
  $lhsCache.ptrAddKey[$kType]($keyCache)
  $contained
}
""")
  }

  def update[K, V](c: Context)(key: c.Expr[K], value: c.Expr[V]): c.Expr[Unit] = {
    import c.universe._
    val (lhs, kType, vType) = findLhsTypeType[AddKeys, Updatable](c)
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[Unit](q"""
{
  val $lhsCache = $lhs
  $lhsCache.ptrUpdate[$vType]($lhsCache.ptrAddKey[$kType]($key), $value)
}
""")
  }


  def containsItem[K, V](c: Context)(key: c.Expr[K], value: c.Expr[V]): c.Expr[Boolean] = {
    import c.universe._
    val (lhs, kType, vType) = findLhsTypeType[Searchable, Values](c)
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[Boolean](q"""
{
  val $lhsCache = $lhs
  $lhsCache.ptrFind[$kType]($key) match {
    case VPtr(vp) => $lhsCache.ptrValue[$vType](vp) == $value
    case _ => false
  }
}
""")
  }

  def mapply[K, V](c: Context)(key: c.Expr[K]): c.Expr[V] = {
    import c.universe._
    val (lhs, kType, vType) = findLhsTypeType[Searchable, Values](c)
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[V](q"""
{
  val $lhsCache = $lhs
  $lhsCache.ptrFind[$kType]($key) match {
    case VPtr(vp) => vp.value[$vType]
    case _ => throw new NoSuchElementException("key not found: " + $key)
  }
}
""")
  }

  def getOrElse[K, V](c: Context)(key: c.Expr[K], fallback: c.Expr[V]): c.Expr[V] = {
    import c.universe._
    val (lhs, kType, vType) = findLhsTypeType[Searchable, Values](c)
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[V](q"""
{
  val $lhsCache = $lhs
  $lhsCache.ptrFind[$kType]($key) match {
    case VPtr(vp) => $lhsCache.ptrValue[$vType](vp)
    case _ => $fallback
  }
}
""")
  }

  def get[K, V](c: Context)(key: c.Expr[K]): c.Expr[Opt[V]] = {
    import c.universe._
    val (lhs, kType, vType) = findLhsTypeType[Searchable, Values](c)
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[Opt[V]](q"""
{
  val $lhsCache = $lhs
  $lhsCache.ptrFind[$kType]($key) match {
    case VPtr(vp) =>
      spire.util.Opt[$vType]($lhsCache.ptrValue[$vType](vp))
    case _ => spire.util.Opt.empty[$vType]
  }
}
""")
  }

}

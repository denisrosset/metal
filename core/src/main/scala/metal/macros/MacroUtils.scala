package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

object MacroUtils {

  def extractPath[T:c.WeakTypeTag](c: Context): c.Symbol = {
    import c.universe._
    implicitly[c.WeakTypeTag[T]].tpe match {
      case TypeRef(SingleType(_, container), _, Nil) => container
      case t => c.abort(c.enclosingPosition, "Cannot extract container value from path dependent type (type = %s)" format t)
    }
  }

  def findLhs(c: Context): c.Tree = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) => lhs
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

   /*
  def extract[T](c: Context)(implicit tagT: c.WeakTypeTag[T]): (c.Symbol, c.Type) = {
    import c.universe._
    tagT.tpe match {
      case TypeRef(containerType@SingleType(_, container), _, Nil) => (container, containerType)
      case t => c.abort(c.enclosingPosition, "Cannot extract container value from path dependent type (type = %s)" format t)
    }
  }
   */

  /*
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

  def findType[TC[_]](c: Context)(lhs: c.Tree)(implicit tc: c.WeakTypeTag[TC[_]]): c.Type = {
    import c.universe._
    val tcClass: ClassSymbol = tc.tpe.typeSymbol.asClass
    val tcTypeParam: Type = tcClass.typeParams(0).asType.toType
    tcTypeParam.asSeenFrom(lhs.tpe, tcClass)
  }

  def findLhsType[TC[_]](c: Context)(implicit tc: c.WeakTypeTag[TC[_]]): (c.Tree, c.Type) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        (lhs, findType[TC](c)(lhs))
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

  def findLhsTypeType[TC1[_], TC2[_]](c: Context)(implicit tc1: c.WeakTypeTag[TC1[_]], tc2: c.WeakTypeTag[TC2[_]]): (c.Tree, c.Type, c.Type) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        val tc1Class: ClassSymbol = tc1.tpe.typeSymbol.asClass
        val tc2Class: ClassSymbol = tc2.tpe.typeSymbol.asClass
        val tc1TypeParam: Type = tc1Class.typeParams(0).asType.toType
        val tc2TypeParam: Type = tc2Class.typeParams(0).asType.toType
        val a1Type: Type = tc1TypeParam.asSeenFrom(lhs.tpe, tc1Class)
        val a2Type: Type = tc2TypeParam.asSeenFrom(lhs.tpe, tc2Class)
        (lhs, a1Type, a2Type)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

  def findLhsTypeTypeType[TC1[_], TC2[_], TC3[_]](c: Context)(implicit tc1: c.WeakTypeTag[TC1[_]], tc2: c.WeakTypeTag[TC2[_]], tc3: c.WeakTypeTag[TC3[_]]): (c.Tree, c.Type, c.Type, c.Type) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) =>
        val tc1Class: ClassSymbol = tc1.tpe.typeSymbol.asClass
        val tc2Class: ClassSymbol = tc2.tpe.typeSymbol.asClass
        val tc3Class: ClassSymbol = tc3.tpe.typeSymbol.asClass
        val tc1TypeParam: Type = tc1Class.typeParams(0).asType.toType
        val tc2TypeParam: Type = tc2Class.typeParams(0).asType.toType
        val tc3TypeParam: Type = tc3Class.typeParams(0).asType.toType
        val a1Type: Type = tc1TypeParam.asSeenFrom(lhs.tpe, tc1Class)
        val a2Type: Type = tc2TypeParam.asSeenFrom(lhs.tpe, tc2Class)
        val a3Type: Type = tc3TypeParam.asSeenFrom(lhs.tpe, tc3Class)
        (lhs, a1Type, a2Type, a3Type)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }*/

}

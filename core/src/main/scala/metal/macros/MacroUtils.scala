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

}

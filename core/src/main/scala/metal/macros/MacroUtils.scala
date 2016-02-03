package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

object MacroUtils {

  def extractSingleton[C <: Singleton:c.WeakTypeTag](c: Context): c.Symbol = {
    import c.universe._
    implicitly[c.WeakTypeTag[C]].tpe match {
      case SingleType(_, container) => container
      case t => c.abort(c.enclosingPosition, "Cannot extract container value from singleton type (type = %s)" format t)
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

package metal
package macros

import spire.macros.compat.Context
import spire.macros.SyntaxUtil

trait Call[C <: Context with Singleton] {

  val c: C

  /** Instantiates a call to the function/tree `body`, using the elements pointed to by the pointer
    * named `pointerName` on container `containerName`.
    */
  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree

  /** Instantiates a call to the function/tree `body`, using the elements pointed to by the pointer
    * named `pointerName` on container `containerName`, providing `value` as a first argument to the 
    * function (i.e. as in `foldLeft`).
    */

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree

}

trait CallElements1[C <: Context with Singleton, E1] extends Call[C] {

  def tagE1: c.WeakTypeTag[E1]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val e1 = util.name("e1")
    q"""
val $e1: $tagE1 = $containerName.ptrElement1[$tagE1]($pointerName)
$body($e1)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val e1 = util.name("e1")
    q"""
val $e1: $tagE1 = $containerName.ptrElement1[$tagE1]($pointerName)
$body($value, $e1)
"""
  }

}

object CallElements1 {
  def apply[C <: Context with Singleton, E1:_c.WeakTypeTag](_c: C): CallElements1[C, E1] =
    new CallElements1[C, E1] {
      val c: C = _c
      def tagE1 = implicitly[c.WeakTypeTag[E1]]
    }
}

trait CallElements2[C <: Context with Singleton, E1, E2] extends Call[C] {

  def tagE1: c.WeakTypeTag[E1]

  def tagE2: c.WeakTypeTag[E2]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val List(e1, e2) = util.names("e1", "e2")
    q"""
val $e1: $tagE1 = $containerName.ptrElement1[$tagE1]($pointerName)
val $e2: $tagE2 = $containerName.ptrElement2[$tagE2]($pointerName)
$body($e1, $e2)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val List(e1, e2) = util.names("e1", "e2")
    q"""
val $e1: $tagE1 = $containerName.ptrElement1[$tagE1]($pointerName)
val $e2: $tagE2 = $containerName.ptrElement2[$tagE2]($pointerName)
$body($value, $e1, $e2)
"""
  }

}

object CallElements2 {

  def apply[C <: Context with Singleton, E1:_c.WeakTypeTag, E2:_c.WeakTypeTag](_c: C): CallElements2[C, E1, E2] =
    new CallElements2[C, E1, E2] {
      val c: C = _c
      def tagE1 = implicitly[c.WeakTypeTag[E1]]
      def tagE2 = implicitly[c.WeakTypeTag[E2]]
    }

}

trait CallElements3[C <: Context with Singleton, E1, E2, E3] extends Call[C] {

  def tagE1: c.WeakTypeTag[E1]

  def tagE2: c.WeakTypeTag[E2]

  def tagE3: c.WeakTypeTag[E3]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val List(e1, e2, e3) = util.names("e1", "e2", "e3")
    q"""
val $e1: $tagE1 = $containerName.ptrElement1[$tagE1]($pointerName)
val $e2: $tagE2 = $containerName.ptrElement2[$tagE2]($pointerName)
val $e3: $tagE3 = $containerName.ptrElement3[$tagE3]($pointerName)
$body($e1, $e2, $e3)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val List(e1, e2, e3) = util.names("e1", "e2", "e3")
    q"""
val $e1: $tagE1 = $containerName.ptrElement1[$tagE1]($pointerName)
val $e2: $tagE2 = $containerName.ptrElement2[$tagE2]($pointerName)
val $e3: $tagE3 = $containerName.ptrElement3[$tagE3]($pointerName)
$body($value, $e1, $e2, $e3)
"""
  }

}

object CallElements3 {

  def apply[C <: Context with Singleton, E1:_c.WeakTypeTag, E2:_c.WeakTypeTag, E3:_c.WeakTypeTag](_c: C): CallElements3[C, E1, E2, E3] =
    new CallElements3[C, E1, E2, E3] {
      val c: C = _c
      def tagE1 = implicitly[c.WeakTypeTag[E1]]
      def tagE2 = implicitly[c.WeakTypeTag[E2]]
      def tagE3 = implicitly[c.WeakTypeTag[E3]]
    }

}

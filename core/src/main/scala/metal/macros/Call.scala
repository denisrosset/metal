package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import MacroUtils._

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

trait CallElements[C <: Context with Singleton, E] extends Call[C] {

  def tagE: c.WeakTypeTag[E]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val e = util.name("$e")
    q"""
val $e: $tagE = $containerName.ptrElement[$tagE]($pointerName)
$body($e)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val e = util.name("$e")
    q"""
val $e: $tagE = $containerName.ptrElement[$tagE]($pointerName)
$body($value, $e)
"""
  }

}

object CallElements {
  def apply[C <: Context with Singleton, E:_c.WeakTypeTag](_c: C): CallElements[C, E] =
    new CallElements[C, E] {
      val c: C = _c
      def tagE = implicitly[c.WeakTypeTag[E]]
    }
}

/** Variant for containers who contain keys only. */
trait CallKeys[C <: Context with Singleton, K] extends Call[C] {

  def tagK: c.WeakTypeTag[K]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val k = util.name("$k")
    q"""
val $k: $tagK = $containerName.ptrKey[$tagK]($pointerName)
$body($k)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val k = util.name("$k")
    q"""
val $k: $tagK = $containerName.ptrKey[$tagK]($pointerName)
$body($value, $k)
"""
  }

}

object CallKeys {
  def apply[C <: Context with Singleton, K:_c.WeakTypeTag](_c: C): CallKeys[C, K] =
    new CallKeys[C, K] {
      val c: C = _c
      def tagK = implicitly[c.WeakTypeTag[K]]
    }

}

/** Variant for containers who contain values only. */
trait CallValues[C <: Context with Singleton, V] extends Call[C] {

  def tagV: c.WeakTypeTag[V]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val v = util.name("$v")
    q"""
val $v: $tagV = $containerName.ptrValue[$tagV]($pointerName)
$body($v)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val v = util.name("$v")
    q"""
val $v: $tagV = $containerName.ptrValue[$tagV]($pointerName)
$body($value, $v)
"""
  }

}

object CallValues {

  def apply[C <: Context with Singleton, V:_c.WeakTypeTag](_c: C): CallValues[C, V] =
    new CallValues[C, V] {
      val c: C = _c
      def tagV = implicitly[c.WeakTypeTag[V]]
    }

}

/** Variant for containers who contain key-value pairs. */
trait CallKeysValues[C <: Context with Singleton, K, V] extends Call[C] {

  def tagK: c.WeakTypeTag[K]

  def tagV: c.WeakTypeTag[V]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val List(k, v) = util.names("$k", "$v")
    q"""
val $k: $tagK = $containerName.ptrKey[$tagK]($pointerName)
val $v: $tagV = $containerName.ptrValue[$tagV]($pointerName)
$body($k, $v)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val List(k, v) = util.names("$k", "$v")
    q"""
val $k: $tagK = $containerName.ptrKey[$tagK]($pointerName)
val $v: $tagV = $containerName.ptrValue[$tagV]($pointerName)
$body($value, $k, $v)
"""
  }

}

object CallKeysValues {

  def apply[C <: Context with Singleton, K:_c.WeakTypeTag, V:_c.WeakTypeTag](_c: C): CallKeysValues[C, K, V] =
    new CallKeysValues[C, K, V] {
      val c: C = _c
      def tagK = implicitly[c.WeakTypeTag[K]]
      def tagV = implicitly[c.WeakTypeTag[V]]
    }

}

/** Variant for containers who contain key-value1-value2 triples. */
trait CallKeysValues1Values2[C <: Context with Singleton, K, V1, V2] extends Call[C] {

  def tagK: c.WeakTypeTag[K]

  def tagV1: c.WeakTypeTag[V1]

  def tagV2: c.WeakTypeTag[V2]

  def apply(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val List(k, v1, v2) = util.names("$k", "$v1", "$v2")
    q"""
val $k: $tagK = $containerName.ptrKey[$tagK]($pointerName)
val $v1: $tagV1 = $containerName.ptrValue1[$tagV1]($pointerName)
val $v2: $tagV2 = $containerName.ptrValue2[$tagV2]($pointerName)
$body($k, $v1, $v2)
"""
  }

  def withValue(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val List(k, v1, v2) = util.names("$k", "$v1", "$v2")
    q"""
val $k: $tagK = $containerName.ptrKey[$tagK]($pointerName)
val $v1: $tagV1 = $containerName.ptrValue1[$tagV1]($pointerName)
val $v2: $tagV2 = $containerName.ptrValue2[$tagV2]($pointerName)
$body($value, $k, $v1, $v2)
"""
  }

}

object CallKeysValues1Values2 {

  def apply[C <: Context with Singleton, K:_c.WeakTypeTag, V1:_c.WeakTypeTag, V2:_c.WeakTypeTag](_c: C): CallKeysValues1Values2[C, K, V1, V2] =
    new CallKeysValues1Values2[C, K, V1, V2] {
      val c: C = _c
      def tagK = implicitly[c.WeakTypeTag[K]]
      def tagV1 = implicitly[c.WeakTypeTag[V1]]
      def tagV2 = implicitly[c.WeakTypeTag[V2]]
    }

}

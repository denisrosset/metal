package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}

import MacroUtils._

trait Call {

  /** Instantiates a call to the function/tree `body`, using the elements pointed to by the pointer
    * named `pointerName` on container `containerName`.
    */
  def apply(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree

  /** Instantiates a call to the function/tree `body`, using the elements pointed to by the pointer
    * named `pointerName` on container `containerName`, providing `value` as a first argument to the 
    * function (i.e. as in `foldLeft`).
    */

  def withValue(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree

}

/** Variant for containers whose element is known. */
object CallE extends Call {

  def apply(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val e = util.name("e")
    val eType = findType[Elements](c)(lhs)
    q"""
val $e: $eType = $containerName.ptrElement[$eType]($pointerName)
$body($e)
"""
  }

  def withValue(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val e = util.name("e")
    val eType = findType[Elements](c)(lhs)
    q"""
val $e: $eType = $containerName.ptrElement[$eType]($pointerName)
$body($value, $e)
"""
  }

}

/** Variant for containers who contain keys only. */
object CallK extends Call {

  def apply(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val k = util.name("k")
    val kType = findType[Keys](c)(lhs)
    q"""
val $k: $kType = $containerName.ptrKey[$kType]($pointerName)
$body($k)
"""
  }

  def withValue(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val k = util.name("k")
    val kType = findType[Keys](c)(lhs)
    q"""
val $k: $kType = $containerName.ptrKey[$kType]($pointerName)
$body($value, $k)
"""
  }

}

/** Variant for containers who contain values only. */
object CallV extends Call {

  def apply(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val v = util.name("v")
    val vType = findType[Values](c)(lhs)
    q"""
val $v: $vType = $containerName.ptrValue[$vType]($pointerName)
$body($v)
"""
  }

  def withValue(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val v = util.name("v")
    val vType = findType[Values](c)(lhs)
    q"""
val $v: $vType = $containerName.ptrValue[$vType]($pointerName)
$body($value, $v)
"""
  }

}

/** Variant for containers who contain key-value pairs only. */
object CallKV extends Call {

  def apply(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val List(k, v) = util.names("k", "v")
    val kType = findType[Keys](c)(lhs)
    val vType = findType[Values](c)(lhs)
    q"""
val $k: $kType = $containerName.ptrKey[$kType]($pointerName)
val $v: $vType = $containerName.ptrValue[$vType]($pointerName)
$body($k, $v)
"""
  }

  def withValue(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val List(k, v) = util.names("k", "v")
    val kType = findType[Keys](c)(lhs)
    val vType = findType[Values](c)(lhs)
    q"""
val $k: $kType = $containerName.ptrKey[$kType]($pointerName)
val $v: $vType = $containerName.ptrValue[$vType]($pointerName)
$body($value, $k, $v)
"""
  }

}

/** Variant for containers who contain key-value1-value2 triples only. */
object CallKV1V2 extends Call {

  def apply(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree): c.Tree = {
    import c.universe._
    val List(k, v1, v2) = util.names("k", "v1", "v2")
    val kType = findType[Keys](c)(lhs)
    val v1Type = findType[Values1](c)(lhs)
    val v2Type = findType[Values2](c)(lhs)
    q"""
val $k: $kType = $containerName.ptrKey[$kType]($pointerName)
val $v1: $v1Type = $containerName.ptrValue1[$v1Type]($pointerName)
val $v2: $v2Type = $containerName.ptrValue2[$v2Type]($pointerName)
$body($k, $v1, $v2)
"""
  }

  def withValue(c: Context)(util: SyntaxUtil[c.type], lhs: c.Tree, containerName: c.TermName, pointerName: c.TermName, body: c.Tree, value: c.TermName): c.Tree = {
    import c.universe._
    val List(k, v1, v2) = util.names("k", "v1", "v2")
    val kType = findType[Keys](c)(lhs)
    val v1Type = findType[Values1](c)(lhs)
    val v2Type = findType[Values2](c)(lhs)
    q"""
val $k: $kType = $containerName.ptrKey[$kType]($pointerName)
val $v1: $v1Type = $containerName.ptrValue1[$v1Type]($pointerName)
val $v2: $v2Type = $containerName.ptrValue2[$v2Type]($pointerName)
$body($value, $k, $v1, $v2)
"""
  }

}

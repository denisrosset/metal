package metal
package macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}
import spire.util.Opt

import MacroUtils._

object Ops {

  def contains[K:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    c.Expr[Boolean](q"$lhs.ptrFind[$tagK]($key).nonNull")
  }

  def add[K:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val util = SyntaxUtil[c.type](c)
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val lhsCache = util.name("lhsCache")
    val keyCache = util.name("keyCache")
    val contained = util.name("contained")
    c.Expr[Boolean](q"""
val $lhsCache = $lhs
val $keyCache = $key
val $contained = $lhsCache.ptrFind[$tagK]($keyCache).nonNull
$lhsCache.ptrAddKey[$tagK]($keyCache)
$contained
""")
    }

  def +=[K:c.WeakTypeTag, T](c: Context)(key: c.Expr[K]): c.Expr[T] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[T](q"""
val $lhsCache = $lhs
$lhsCache.ptrAddKey[$tagK]($key)
$lhsCache
""")
  }

  def remove[K:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[Boolean](q"""
val $lhsCache = $lhs
val $ptr: Ptr[$lhsCache.Tag, $lhsCache.Cap] = $lhsCache.ptrFind[$tagK]($key)
if ($ptr.nonNull) {
  $lhsCache.ptrRemove(new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
  true
} else false
""")
  }

  def -=[K:c.WeakTypeTag, T](c: Context)(key: c.Expr[K]): c.Expr[T] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[T](q"""
val $lhsCache = $lhs
val $ptr: Ptr[$lhsCache.Tag, $lhsCache.Cap] = $lhsCache.ptrFind[$tagK]($key)
if ($ptr.nonNull)
  $lhsCache.ptrRemove(new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
$lhsCache
""")
  }

  def update[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(key: c.Expr[K], value: c.Expr[V]): c.Expr[Unit] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV = implicitly[c.WeakTypeTag[V]]
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    c.Expr[Unit](q"""
val $lhsCache = $lhs
$lhsCache.ptrUpdate[$tagV]($lhsCache.ptrAddKey[$tagK]($key), $value)
""")
  }

  def containsItem[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(key: c.Expr[K], value: c.Expr[V]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV = implicitly[c.WeakTypeTag[V]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[Boolean](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) 
  ($lhsCache.ptrValue[$tagV](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw)) == $value)
else
  false
""")
  }

  def containsItem2[K:c.WeakTypeTag, V1:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(key: c.Expr[K], value1: c.Expr[V1], value2: c.Expr[V2]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV1 = implicitly[c.WeakTypeTag[V1]]
    val tagV2 = implicitly[c.WeakTypeTag[V2]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr, vp) = util.names("$lhsCache", "$ptr", "$vp")
    c.Expr[Boolean](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) {
  val $vp = new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw)
  ($lhsCache.ptrValue1[$tagV1]($vp) == $value1) &&
  ($lhsCache.ptrValue2[$tagV2]($vp) == $value2)
} else false
""")
  }

  def apply[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[V] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV = implicitly[c.WeakTypeTag[V]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[V](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) 
  $lhsCache.ptrValue[$tagV](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
else
  throw new NoSuchElementException("key not found: " + $key)
""")
  }

  def getOrElse[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(key: c.Expr[K], fallback: c.Expr[V]): c.Expr[V] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV = implicitly[c.WeakTypeTag[V]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[V](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) 
  $lhsCache.ptrValue[$tagV](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
else
  $fallback
""")
  }

  def get[K:c.WeakTypeTag, V:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[Opt[V]] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV = implicitly[c.WeakTypeTag[V]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[Opt[V]](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull)
  _root_.spire.util.Opt[$tagV]($lhsCache.ptrValue[$tagV](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw)))
else
  _root_.spire.util.Opt.empty[$tagV]
""")
  }

  def update2[K:c.WeakTypeTag, V1:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(key: c.Expr[K], value: c.Expr[(V1, V2)]): c.Expr[Unit] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV1 = implicitly[c.WeakTypeTag[V1]]
    val tagV2 = implicitly[c.WeakTypeTag[V2]]
    val util = SyntaxUtil[c.type](c)
    val lhsCache = util.name("$lhsCache")
    val ptr = util.name("$ptr")
    value.tree match {
      case Apply(TypeApply(Select(Select(Ident(_), tuple2Name), TermName("apply")), Seq(_, _)), Seq(value1, value2)) =>
        c.Expr[Unit](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrAddKey[$tagK]($key)
$lhsCache.ptrUpdate1[$tagV1]($ptr, $value1)
$lhsCache.ptrUpdate2[$tagV2]($ptr, $value2)
""")
      case _ =>
        c.Expr[Unit](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrAddKey[$tagK]($key)
$lhsCache.ptrUpdate1[$tagV1]($ptr, $value._1)
$lhsCache.ptrUpdate2[$tagV2]($ptr, $value._2)
""")
    }
  }

  def apply1[K:c.WeakTypeTag, V1:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[V1] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV1 = implicitly[c.WeakTypeTag[V1]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[V1](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) 
  $lhsCache.ptrValue1[$tagV1](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
else
  throw new NoSuchElementException("key not found: " + $key)
""")
  }

  def getOrElse1[K:c.WeakTypeTag, V1:c.WeakTypeTag](c: Context)(key: c.Expr[K], fallback: c.Expr[V1]): c.Expr[V1] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV1 = implicitly[c.WeakTypeTag[V1]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[V1](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) 
  $lhsCache.ptrValue1[$tagV1](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
else
  $fallback
""")
  }

  def get1[K:c.WeakTypeTag, V1:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[Opt[V1]] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV1 = implicitly[c.WeakTypeTag[V1]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[Opt[V1]](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull)
  _root_.spire.util.Opt[$tagV1]($lhsCache.ptrValue1[$tagV1](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw)))
else
  _root_.spire.util.Opt.empty[$tagV1]
""")
  }

  def apply2[K:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[V2] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV2 = implicitly[c.WeakTypeTag[V2]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[V2](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) 
  $lhsCache.ptrValue2[$tagV2](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
else
  throw new NoSuchElementException("key not found: " + $key)
""")
  }

  def getOrElse2[K:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(key: c.Expr[K], fallback: c.Expr[V2]): c.Expr[V2] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV2 = implicitly[c.WeakTypeTag[V2]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[V2](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull) 
  $lhsCache.ptrValue2[$tagV2](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw))
else
  $fallback
""")
  }

  def get2[K:c.WeakTypeTag, V2:c.WeakTypeTag](c: Context)(key: c.Expr[K]): c.Expr[Opt[V2]] = {
    import c.universe._
    val lhs = findLhs(c)
    val tagK = implicitly[c.WeakTypeTag[K]]
    val tagV2 = implicitly[c.WeakTypeTag[V2]]
    val util = SyntaxUtil[c.type](c)
    val List(lhsCache, ptr) = util.names("$lhsCache", "$ptr")
    c.Expr[Opt[V2]](q"""
val $lhsCache = $lhs
val $ptr = $lhsCache.ptrFind[$tagK]($key) 
if ($ptr.nonNull)
  _root_.spire.util.Opt[$tagV2]($lhsCache.ptrValue2[$tagV2](new VPtr[$lhsCache.Tag, $lhsCache.Cap]($ptr.raw)))
else
  _root_.spire.util.Opt.empty[$tagV2]
""")
  }

}

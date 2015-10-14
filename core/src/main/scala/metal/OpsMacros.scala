package metal

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}
import spire.macros.{SyntaxUtil, InlineUtil}
import spire.util.Opt

object OpsMacrosP {

  def findEvLhs[T[_], A](c: Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(lhs)), List(ev)) => (ev, lhs)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

  def findEvKEvVLhs[T[_], A](c: Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(Apply(TypeApply(_, _), List(lhs)), List(evK, evV)) => (evK, evV, lhs)
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

  def contains[K](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val (ev, lhs) = findEvLhs(c)
    c.Expr[Boolean](q"$lhs.ptrFindP($ev.encode($key))($ev).nonNull")
  }

  def remove[K](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val (ev, lhs) = findEvLhs(c)
    c.Expr[Boolean](q"""
$lhs.ptrFindP($ev.encode($key))($ev) match { 
  case VPtr(vp) => 
    $lhs.ptrRemove(vp)
    true
  case _ => false
}""")
  }

  def -=[K, T](c: Context)(key: c.Expr[K]): c.Expr[T] = {
    import c.universe._
    val (ev, lhs) = findEvLhs(c)
    c.Expr[T](q"""
$lhs.ptrFindP($ev.encode($key))($ev) match { 
  case VPtr(vp) => 
    $lhs.ptrRemove(vp)
    $lhs
  case _ => 
    $lhs 
}
""")
  }

  def +=[K, T](c: Context)(key: c.Expr[K]): c.Expr[T] = {
    import c.universe._
    val (ev, lhs) = findEvLhs(c)
    c.Expr[T](q"$lhs.ptrAddKeyP($ev.encode($key)); $lhs")
  }

  def add[K](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val util = SyntaxUtil[c.type](c)
    val (ev, lhs) = findEvLhs(c)
    val encoded = util.name("encoded")
    val contained = util.name("contained")
    val tree = q"""
{
  val $encoded = $ev.encode($key)
  val $contained = $lhs.ptrFindP($encoded).nonNull
  $lhs.ptrAddKeyP($encoded)
  $contained
}
"""
    c.Expr[Boolean](tree)
  }
  //     new InlineUtil[c.type](c).inlineAndReset[Unit](tree)
  def update[K, V](c: Context)(key: c.Expr[K], value: c.Expr[V]): c.Expr[Unit] = {
    import c.universe._
    val (evK, evV, lhs) = findEvKEvVLhs(c)
    val tree = q"$lhs.ptrUpdateP($lhs.ptrAddKeyP($evK.encode($key)), $evV.encode($value))"
    c.Expr[Unit](tree)
  }

  def containsItem[K, V](c: Context)(key: c.Expr[K], value: c.Expr[V]): c.Expr[Boolean] = {
    import c.universe._
    val (evK, evV, lhs) = findEvKEvVLhs(c)
    val tree = q"""
$lhs.ptrFindP($evK.encode($key)) match {
  case VPtr(vp) => $evV.decode($lhs.ptrValueP(vp)) == $value
  case _ => false
}
"""
    c.Expr[Boolean](tree)
  }

  def apply[K, V](c: Context)(key: c.Expr[K]): c.Expr[V] = {
    import c.universe._
    val (evK, evV, lhs) = findEvKEvVLhs(c)
    val tree = q"""
$lhs.ptrFindP($evK.encode($key)) match {
  case VPtr(vp) => $evV.decode($lhs.ptrValueP(vp))
  case _ => throw new NoSuchElementException("key not found: " + $key)
}
"""
    c.Expr[V](tree)
  }

  def getOrElse[K, V](c: Context)(key: c.Expr[K], fallback: c.Expr[V]): c.Expr[V] = {
    import c.universe._
    val (evK, evV, lhs) = findEvKEvVLhs(c)
    val tree = q"""
$lhs.ptrFindP($evK.encode($key)) match {
  case VPtr(vp) => $evV.decode($lhs.ptrValueP(vp))
  case _ => $fallback
}
"""
    c.Expr[V](tree)
  }

  def get[K, V](c: Context)(key: c.Expr[K]): c.Expr[Opt[V]] = {
    import c.universe._
    val (evK, evV, lhs) = findEvKEvVLhs(c)
    val util = SyntaxUtil[c.type](c)
    val v = util.name("v")
    val tree = q"""
$lhs.ptrFindP($evK.encode($key)) match {
  case VPtr(vp) =>
    val $v: V = $evV.decode($lhs.ptrValue(vp))
    Opt[V]($v)
  case _ => Opt.empty[V]
}
"""
    c.Expr[Opt[V]](tree)
  }

}

object OpsMacros {

  def findLhs[A](c: Context) = {
    import c.universe._
    c.prefix.tree match {
      case Apply(TypeApply(_, _), List(lhs)) => lhs
      case t => c.abort(c.enclosingPosition, "Cannot extract subject of operation (tree = %s)" format t)
    }
  }

/*  def remove[K](c: Context)(key: c.Expr[K]): c.Expr[Boolean] = {
    import c.universe._
    val lhs = findLhs(c)
    c.Expr[Boolean](q"$lhs.ptrFind($key) match { case VPtr(vp) => vp.remove; true; case _ => false }")
  }

  def -=[K, L](c: Context)(key: c.Expr[K]): c.Expr[L] = {
    import c.universe._
    val lhs = findLhs(c)
    c.Expr[L](q"$lhs.ptrFind($key) match { case VPtr(vp) => vp.remove; $lhs; case _ => $lhs }")
 }  */

}

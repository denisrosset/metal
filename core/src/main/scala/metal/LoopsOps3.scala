package metal

import spire.algebra._
import spire.util.Opt

final class LoopOps3[E1, E2, E3](val lhs: Enumerable with Elements3[E1, E2, E3]) {

  def foreach(body: (E1, E2, E3) => Unit): Unit = macro macros.Loops.foreach3[E1, E2, E3]

  def count(body: (E1, E2, E3) => Boolean): Long = macro macros.Loops.count3[E1, E2, E3]

  def exists(body: (E1, E2, E3) => Boolean): Boolean = macro macros.Loops.exists3[E1, E2, E3]

  def forall(body: (E1, E2, E3) => Boolean): Boolean = macro macros.Loops.forall3[E1, E2, E3]

  def foldLeft[A](initialValue: A)(body: (A, E1, E2, E3) => A): A = macro macros.Loops.foldLeft3[A, E1, E2, E3]

  def /:[A](initialValue: A)(body: (A, E1, E2, E3) => A): A = macro macros.Loops.foldLeft3[A, E1, E2, E3]

}

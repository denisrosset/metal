package metal

final class LoopOps2[E1, E2](val lhs: Enumerable with NElements2[E1, E2]) {

  def foreach(body: (E1, E2) => Unit): Unit = macro macros.Loops.foreach2[E1, E2]

  def count(body: (E1, E2) => Boolean): Long = macro macros.Loops.count2[E1, E2]

  def exists(body: (E1, E2) => Boolean): Boolean = macro macros.Loops.exists2[E1, E2]

  def forall(body: (E1, E2) => Boolean): Boolean = macro macros.Loops.forall2[E1, E2]

  def foldLeft[A](initialValue: A)(body: (A, E1, E2) => A): A = macro macros.Loops.foldLeft2[A, E1, E2]

  def /:[A](initialValue: A)(body: (A, E1, E2) => A): A = macro macros.Loops.foldLeft2[A, E1, E2]

}

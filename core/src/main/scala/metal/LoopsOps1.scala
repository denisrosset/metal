package metal

import spire.algebra.{AdditiveMonoid, MultiplicativeMonoid, Order}

final class LoopOps1[E1](val lhs: Enumerable with NElements1[E1]) {

  def foreach(body: E1 => Unit): Unit = macro macros.Loops.foreach1[E1]

  def count(body: E1 => Boolean): Long = macro macros.Loops.count1[E1]

  def exists(body: E1 => Boolean): Boolean = macro macros.Loops.exists1[E1]

  def forall(body: E1 => Boolean): Boolean = macro macros.Loops.forall1[E1]

  def foldLeft[A](initialValue: A)(body: (A, E1) => A): A = macro macros.Loops.foldLeft1[A, E1]

  def /:[A](initialValue: A)(body: (A, E1) => A): A = macro macros.Loops.foldLeft1[A, E1]

  def min(implicit order: Order[E1]): E1 = macro macros.Loops.min1[E1]

  def max(implicit order: Order[E1]): E1 = macro macros.Loops.max1[E1]

  def sum(implicit am: AdditiveMonoid[E1]): E1 = macro macros.Loops.sum1[E1]

  def product(implicit mm: MultiplicativeMonoid[E1]): E1 = macro macros.Loops.product1[E1]

}

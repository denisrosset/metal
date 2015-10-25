package metal

import spire.algebra._
import spire.util.Opt

final class LoopOpsE[E](val lhs: Enumerable with Elements[E]) {

  def foreach(body: E => Unit): Unit = macro macros.Loops.foreachE[E]

  def count(body: E => Boolean): Long = macro macros.Loops.countE[E]

  def exists(body: E => Boolean): Boolean = macro macros.Loops.existsE[E]

  def forall(body: E => Boolean): Boolean = macro macros.Loops.forallE[E]

  def foldLeft[A](initialValue: A)(body: (A, E) => A): A = macro macros.Loops.foldLeftE[A, E]

  def /:[A](initialValue: A)(body: (A, E) => A): A = macro macros.Loops.foldLeftE[A, E]

  def sum(implicit am: AdditiveMonoid[E]): E = macro macros.Loops.sumE[E]

  def product(implicit mm: MultiplicativeMonoid[E]): E = macro macros.Loops.productE[E]

  def min(implicit order: Order[E]): E = macro macros.Loops.minE[E]

  def max(implicit order: Order[E]): E = macro macros.Loops.maxE[E]

}



final class LoopOpsK[K](val lhs: Enumerable with ElementsK[K]) {

  def foreach(body: K => Unit): Unit = macro macros.Loops.foreachK[K]

  def count(body: K => Boolean): Long = macro macros.Loops.countK[K]

  def exists(body: K => Boolean): Boolean = macro macros.Loops.existsK[K]

  def forall(body: K => Boolean): Boolean = macro macros.Loops.forallK[K]

  def foldLeft[A](initialValue: A)(body: (A, K) => A): A = macro macros.Loops.foldLeftK[A, K]

  def /:[A](initialValue: A)(body: (A, K) => A): A = macro macros.Loops.foldLeftK[A, K]

}

final class LoopOpsV[V](val lhs: Enumerable with ElementsV[V]) {

  def foreach(body: V => Unit): Unit = macro macros.Loops.foreachV[V]

  def count(body: V => Boolean): Long = macro macros.Loops.countV[V]

  def exists(body: V => Boolean): Boolean = macro macros.Loops.existsV[V]

  def forall(body: V => Boolean): Boolean = macro macros.Loops.forallV[V]

  def foldLeft[A](initialValue: A)(body: (A, V) => A): A = macro macros.Loops.foldLeftV[A, V]

  def /:[A](initialValue: A)(body: (A, V) => A): A = macro macros.Loops.foldLeftV[A, V]

}


final class LoopOpsKV[K, V](val lhs: Enumerable with ElementsKV[K, V]) {

  def foreach(body: (K, V) => Unit): Unit = macro macros.Loops.foreachKV[K, V]
  def foreach(body: ((K, V)) => Unit): Unit = macro macros.Loops.foreachE[(K, V)]

  def count(body: (K, V) => Boolean): Long = macro macros.Loops.countKV[K, V]
  def count(body: ((K, V)) => Boolean): Long = macro macros.Loops.countE[(K, V)]

  def exists(body: (K, V) => Boolean): Boolean = macro macros.Loops.existsKV[K, V]
  def exists(body: ((K, V)) => Boolean): Boolean = macro macros.Loops.existsE[(K, V)]

  def forall(body: (K, V) => Boolean): Boolean = macro macros.Loops.forallKV[K, V]
  def forall(body: ((K, V)) => Boolean): Boolean = macro macros.Loops.forallE[(K, V)]

  def foldLeft[A](initialValue: A)(body: (A, K, V) => A): A = macro macros.Loops.foldLeftKV[A, K, V]
  def foldLeft[A](initialValue: A)(body: (A, (K, V)) => A): A = macro macros.Loops.foldLeftE[A, (K, V)]

  def /:[A](initialValue: A)(body: (A, K, V) => A): A = macro macros.Loops.foldLeftKV[A, K, V]
  def /:[A](initialValue: A)(body: (A, (K, V)) => A): A = macro macros.Loops.foldLeftE[A, (K, V)]

}

final class LoopOpsKV1V2[K, V1, V2](val lhs: Enumerable with ElementsKV1V2[K, V1, V2]) {

  def foreach(body: (K, V1, V2) => Unit): Unit = macro macros.Loops.foreachKV1V2[K, V1, V2]
  def foreach(body: ((K, V1, V2)) => Unit): Unit = macro macros.Loops.foreachE[(K, V1, V2)]

  def count(body: (K, V1, V2) => Boolean): Long = macro macros.Loops.countKV1V2[K, V1, V2]
  def count(body: ((K, V1, V2)) => Boolean): Long = macro macros.Loops.countE[(K, V1, V2)]

  def exists(body: (K, V1, V2) => Boolean): Boolean = macro macros.Loops.existsKV1V2[K, V1, V2]
  def exists(body: ((K, V1, V2)) => Boolean): Boolean = macro macros.Loops.existsE[(K, V1, V2)]

  def forall(body: (K, V1, V2) => Boolean): Boolean = macro macros.Loops.forallKV1V2[K, V1, V2]
  def forall(body: ((K, V1, V2)) => Boolean): Boolean = macro macros.Loops.forallE[(K, V1, V2)]

  def foldLeft[A](initialValue: A)(body: (A, K, V1, V2) => A): A = macro macros.Loops.foldLeftKV1V2[A, K, V1, V2]
  def foldLeft[A](initialValue: A)(body: (A, (K, V1, V2)) => A): A = macro macros.Loops.foldLeftE[A, (K, V1, V2)]

  def /:[A](initialValue: A)(body: (A, K, V1, V2) => A): A = macro macros.Loops.foldLeftKV1V2[A, K, V1, V2]
  def /:[A](initialValue: A)(body: (A, (K, V1, V2)) => A): A = macro macros.Loops.foldLeftE[A, (K, V1, V2)]

}

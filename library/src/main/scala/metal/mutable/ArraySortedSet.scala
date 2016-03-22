package metal
package mutable

import spire.algebra.Order
import spire.syntax.cfor._

import generic.Methods

final class ArraySortedSet[K](var items: Array[K], var longSize: Long)(implicit val K: Methods[K], val order: Order[K]) extends generic.ArraySortedSet[K] with mutable.SortedSet[K] {

  def toImmutable: Immutable = new metal.immutable.ArraySortedSet(items.clone, longSize) // TODO: trim the array
  def result(): Immutable = {
    val res = new metal.immutable.ArraySortedSet(items, longSize)(K, order)
    // clears this
    items = K.newArray(0)
    longSize = 0
    res
  }

  def absorb(newItems: Array[K], newSize: Long): Unit = {
    items = newItems
    longSize = newSize
  }

  def clear(): Unit = {
    absorb(K.newArray(0), 0)
  }

  def reset(): Unit = {
    cforRange(0 until longSize.toInt) { i =>
      items(i) = null.asInstanceOf[K]
    }
    longSize = 0
  }

  final def ptrRemoveAndAdvance(ptr: VPtr[this.type]): Ptr[this.type] = {
    val pos = ptr.raw.toInt
    java.lang.System.arraycopy(items, pos + 1, items, pos, longSize.toInt - pos - 1)
    longSize -= 1
    items(longSize.toInt) = null.asInstanceOf[K]
    if (pos >= longSize) Ptr.Null(this) else ptr
  }

  final def ptrRemove(ptr: VPtr[this.type]): Unit = ptrRemoveAndAdvance(ptr)

  @inline final def ptrAddKey[@specialized L](key: L): VPtr[this.type] = {
    val itemsL = items.asInstanceOf[Array[L]]
    val pos = findWhere[L](key)
    if (pos < 0) {
      val ipos = ~pos
      val newItemsL = if (longSize < itemsL.length) itemsL else {
        val arr = (K.newArray(itemsL.length.max(1) * 2)).asInstanceOf[Array[L]]
        java.lang.System.arraycopy(itemsL, 0, arr, 0, ipos)
        arr
      }
      java.lang.System.arraycopy(itemsL, ipos, newItemsL, ipos + 1, longSize.toInt - ipos)
      newItemsL(ipos) = key
      items = newItemsL.asInstanceOf[Array[K]]
      longSize += 1
      VPtr(this, ipos)
    } else VPtr(this, pos)
  }

}

object ArraySortedSet extends metal.mutable.SetFactory {

  type Extra[K] = Order[K]
  type S[K] = metal.mutable.ArraySortedSet[K]

  def reservedSize[K:Methods:Order](n: Int): S[K] = {
    val K = Methods[K]
    import K.classTag
    new metal.mutable.ArraySortedSet[K](K.newArray(n), 0L)
  }

}

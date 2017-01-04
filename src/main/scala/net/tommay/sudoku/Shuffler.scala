package net.tommay.sudoku

import scala.util.Random

// This is fairly horrible.  I want to be able to Stream shuffled
// elements from an Iterable without materializing a shuffled Array
// because often only the first, or just a few, elements are used.  So
// this uses a private mutable Array.

object Shuffler {
  import scala.reflect.ClassTag

  def shuffle[T:ClassTag](rnd: Random, list: Iterable[T]) : Stream[T] = {
    new Shuffler(rnd, list.toArray[T]).shuffle
  }

  class Shuffler[T](rnd: Random, array: Array[T]) {
    var index = array.size

    def shuffle : Stream[T] = {
      if (index != array.size) {
        println(s"$index, ${array.size}")
      }
      if (index == 0) {
        Stream.empty
      }
      else {
        val r = rnd.nextInt(index)
        index -= 1
        val v = array(r)
        array(r) = array(index)
        v #:: shuffle
      }
    }
  }
}

package net.tommay.sudoku

import scala.util.Random

class Util {
}

object Util {
  // Slice an list up into sub-lists of n elements, and return the
  // sub-lists in a list.

  def slices[T](n: Int, list: Seq[T]) : List[Seq[T]] = {
    list.grouped(n).toList
  }

  // This is not pure functional.  The sudoku code may be written so
  // that doesn't matter nuch.
  def split(rnd: Random) : (Random, Random) = {
    val rnd2 = new Random(rnd.nextInt)
    (rnd, rnd2)
  }
}

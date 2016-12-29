package net.tommay.sudoku

object Util {
  // Slice an list up into sub-lists of n elements, and return the
  // sub-lists in a list.

  def slices[T](n: Int, list: Seq[T]) : List[Seq[T]] = {
    list.grouped(n).toList
  }
}

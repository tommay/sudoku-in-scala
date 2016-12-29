package net.tommay.sudoku

object Util {
  // Slice an list up into sub-lists of n elements, and return the
  // sub-lists in a list.

  def slices[T](n: Int, list: Seq[T]) : List[Seq[T]] = {
    list.grouped(n).toList
  }

  def minBy[T](list: Iterable[T], func: T => Ordered[Any]) : T = {
    val enhanced = list.map(e => (func(e), e))
    val minEnhanced = enhanced.tail.foldLeft(enhanced.head) {
      case (a@(na, _), b@(nb, _)) =>
	// xxx choose a or b on ==?
	if (na < nb) a else b
    }
    minEnhanced._2
  }
}

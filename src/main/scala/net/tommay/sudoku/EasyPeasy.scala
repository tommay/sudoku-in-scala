package net.tommay.sudoku

// Given a stripe pf three columns (or rows) col0, col1, and col2.
// Determine the digits that are common to col1 and col2.  For each digit,
// If there is only one Unknown in col0 where the digit is possible, that's
// an EasyPeasy placement.

// Count the occurences of each digit in the entire stripe.  If the
// digit has two occurences, check col0/col1/col2 to see if there's
// only one unknown where the digit is possible.

object EasyPeasy {
  // Easy peasies are found by using stripes of three rows or columns.
  // The first item in the tuple is the set we're looking to place a
  // digit in.  The Iterable has the other two rows/columns in the
  // stripe.  Here we build all the possible stripes so they can be
  // searched for easy peasies.

  val easyPeasyStripes : Stream[(ExclusionSet, ExclusionSet, ExclusionSet)] = {
    Util.slices(3, (ExclusionSet.rows ++ ExclusionSet.columns))
      .toStream
      .flatMap(makeEasyPeasyStripe)
  }

  def makeEasyPeasyStripe(slice: Iterable[ExclusionSet])
    : Iterable[(ExclusionSet, ExclusionSet, ExclusionSet)] =
  {
    slice.map{set =>
      val others = slice.filter(_ != set)
      (set, others.head, others.tail.head)
    }
  }

  // Return a Stream of all possible easy peasy placements for the Puzzle.

  def find(puzzle: Puzzle, unknowns: Iterable[Unknown]) : Stream[Next] = {
    easyPeasyStripes.flatMap(findForEasyPeasyStripe(puzzle, unknowns))
  }

  // Returns any easy peasies in the Puzzle and EasyPeasyStripe.  All
  // digits are considered

  def findForEasyPeasyStripe
    (puzzle: Puzzle, unknowns: Iterable[Unknown])
    (stripe: (ExclusionSet, ExclusionSet, ExclusionSet))
    : Stream[Next] =
  {
    val (col0, col1, col2) = stripe
    val digitsInCol1 = getDigitsInSet(puzzle, col1)
    val digitsInCol2 = getDigitsInSet(puzzle, col2)
    val easyPeasyDigits = digitsInCol1 & digitsInCol2
    easyPeasyDigits.toStream.flatMap(placeDigitInSet(unknowns, col0))
  }

  def getDigitsInSet(puzzle: Puzzle, set: ExclusionSet) : Set[Int] = {
    val cells = set.cells
    // I tried toStream before filtering so the filtered list wouldn't
    // have to be materialized but performance was much worse.
    val placedInSet = puzzle.each.filter{case (cellNumber, _) => cells.contains(cellNumber)}
    // I tried folding into and returning a scala.collection.mutable.Set,
    // but it was a lot slower.
    placedInSet.foldLeft(Set[Int]()){case (accum, (_, digit)) => accum + digit}
  }

  def placeDigitInSet
    (unknowns: Iterable[Unknown], set: ExclusionSet)
    (digit: Int)
    : Stream[Next] =
  {
    val unknownsInSet = Solver.unknownsInSet(unknowns.toStream, set.cells)
    unknownsInSet.filter(_.isDigitPossible(digit)) match {
      case Stream(unknown) =>
        Stream(Next(s"Easy peasy ${set.name}", Placement(unknown.cellNumber, digit)))
      case _ => Stream.empty
    }
  }
}

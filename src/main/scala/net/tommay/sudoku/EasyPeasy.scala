package net.tommay.sudoku

// Given a Stripe pf three columns (or rows) col0, col1, and col2.
// Determine the digits that are common to col1 and col2.  For each digit,
// if there is only one Unknown in col0 where the digit is possible, that's
// an EasyPeasy placement.

case class Stripe (
  col0: ExclusionSet,
  col1: ExclusionSet,
  col2: ExclusionSet)
{
}

object EasyPeasy {
  // Easy peasies are found by using stripes of three rows or columns.
  // The first item in the tuple is the set we're looking to place a
  // digit in.  The Iterable has the other two rows/columns in the
  // stripe.  Here we build all the possible stripes so they can be
  // searched for easy peasies.

  val stripes : Stream[Stripe] = {
    Util.slices(3, (ExclusionSet.rows ++ ExclusionSet.columns))
      .toStream
      .map(_.toStream)
      .flatMap(makeStripe)
  }

  def makeStripe(slice: Iterable[ExclusionSet]) : Iterable[Stripe] =
  {
    slice.map{set =>
      val others = slice.filter(_ != set)
      Stripe(set, others.head, others.tail.head)
    }
  }

  // Return a Stream of all possible easy peasy placements for the Puzzle.

  def find(puzzle: Puzzle, unknowns: Iterable[Unknown]) : Stream[Next] = {
    stripes.flatMap(findForEasyPeasyStripe(puzzle, unknowns))
  }

  // Returns any easy peasies in the Puzzle and EasyPeasyStripe.  All
  // digits are considered

  def findForEasyPeasyStripe
    (puzzle: Puzzle, unknowns: Iterable[Unknown])
    (stripe: Stripe)
    : Stream[Next] =
  {
    val digitsInCol1 = getDigitsInSet(puzzle, stripe.col1)
    val digitsInCol2 = getDigitsInSet(puzzle, stripe.col2)
    val easyPeasyDigits = digitsInCol1 & digitsInCol2
    easyPeasyDigits.toStream.flatMap(placeDigitInSet(unknowns, stripe.col0))
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

package net.tommay.sudoku

// Given a Stripe pf three columns (or rows) col0, col1, and col2.
// Count the occurences of each digit in the entire Stripe.  For each
// digit with two occurences, check col0/col1/col2 to see if there's
// only one Unknown where the digit is possible.

case class Stripe(
  cells: Set[Int],
  exclusionSets: Stream[ExclusionSet])
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
      .map(makeStripe)
  }

  def makeStripe(exclusionSets: Stream[ExclusionSet]) : Stripe = {
    val allCells = exclusionSets.foldLeft(Set.empty[Int]){
      case (accum, exclusionSet) => accum ++ exclusionSet.cells
    }
    Stripe(allCells, exclusionSets)
  }

  // Return a Stream of all possible easy peasy placements for the Puzzle.

  def find(puzzle: Puzzle, unknowns: Iterable[Unknown]) : Stream[Next] = {
    stripes.flatMap(findForEasyPeasyStripe(puzzle, unknowns))
  }

  // Returns any easy peasies in the Puzzle and Stripe.  All digits
  // are considered

  def findForEasyPeasyStripe
    (puzzle: Puzzle, unknowns: Iterable[Unknown])
    (stripe: Stripe)
    : Stream[Next] =
  {
    val doubleDigits =
      countDigitsInSet(puzzle, stripe.cells)
        .toStream
        // XXX flatMap vs. filter + map
        .filter{case (_, list) => list.size == 2}
        .map{case (digit, _) => digit}
    stripe.exclusionSets.flatMap(blah(unknowns, doubleDigits))
  }

  def blah
    (unknowns: Iterable[Unknown], digits: Stream[Int])
    (exclusionSet: ExclusionSet)
      : Stream[Next] =
  {
    val unknownsInSet = Solver.unknownsInSet(
      unknowns.toStream, exclusionSet.cells)
    digits.flatMap(placeDigitInSet(unknownsInSet, exclusionSet))
  }

  def placeDigitInSet
    (unknownsInSet: Iterable[Unknown], set: ExclusionSet)
    (digit: Int)
    : Stream[Next] =
  {
    unknownsInSet.filter(_.isDigitPossible(digit)) match {
      case Stream(unknown) =>
        Stream(Next(s"Easy peasy ${set.name}",
                    Placement(unknown.cellNumber, digit)))
      case _ => Stream.empty
    }
  }

  def countDigitsInSet(
    puzzle: Puzzle, cells: Set[Int])
    : Map[Int, Iterable[Int]] =
  {
    val digitsInSet = puzzle.each
      .filter{case (cellNumber, _) => cells.contains(cellNumber)}
      .map{case (_, digit) => digit}
    digitsInSet.groupBy(identity)
  }
}

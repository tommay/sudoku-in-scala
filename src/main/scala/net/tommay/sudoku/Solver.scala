package net.tommay.sudoku

case class Solver (
  val options: SolverOptions,
  val rnd: Option[Random],
  val puzzle: Puzzle,
  val unknowns: List[Unknown] = List(),
  val steps: List[Step] = List())	// xxx need initial puzzle step
{
  def solutionsTop : Iterable[Solution] = {
    unknowns match {
      case Nil =>
	// No more unknowns, solved!
	Stream(Solution(puzzle, steps))
      case _ =>
	// Carry on with solutionsHeuristic
	solutionsHeuristic
    }
  }

  def solutionsHeuristic : Iterable[Solution] = {
    solutionsStuck
  }

  def solutionsStuck : Iterable[Solution] = {
    // We get here because we can't place a digit using human-style
    // heuristics, so we've either failed or we have to guess and
    // recurse.  We can distinguish by examining the cell with the
    // fewest possibilities remaining, which is also the best cell to
    // make a guess for.

    val minUnknown = Util.minBy(unknowns, Unknown.numPossible)
    val cellNumber = minUnknown.cellNumber
    // This is a List:
    val possible = minUnknown.getPossible
    possible match {
      case Nil =>
	// Failed.  No solutions.
	Stream.empty
      case List(digit) =>
	// One possibility.  The choice is forced, no guessing.  But
	// we only use the force if a) we're guessing, b) we're not
	// using heuristics, because if we are then forcing is done by
	// findForced.
	if (options.useGuessing && !options.useHeuristics) {
          val next = Next("Forced guess", digit, cellNumber)
          placeAndContinue(next)
	}
        else {
	  // There is a forced guess but we're not configured to use
	  // it.  See if we can apply a TrickySet to create an
	  // opportunity.
          applyOneTrickySetIfAllowed match {
            case Some(newSolver) => newSolver.solutionsTop
            case _ => Stream.empty
	  }
	}
      case  _ =>
	// Multiple possibilities.  Before we guess, see if it's
	// possible to permanently apply a TrickySet to create
	// possibiities for heurisstics.
	applyOneTrickySetIfAllowed match {
          case Some(newSolver) => newSolver.solutionsTop
          case _ =>
	    if (options.useGuessing) {
              // Guess each possibility, maybe in a random order, and
              // recurse.  We could use Random.split when shuffling or
              // recursing, but it's not really important for this
              // application.
              val shuffledPossible = maybeShuffle(rnd, possible)
              doGuesses(cellNumber, shuffledPossible)
	    }
	    else {
	      Stream.empty
	    }
	}
    }
  }

  // For each digit in the list, use it as a guess for unknown
  // and try to solve the resulting Puzzle.
  // XXX How to do this with : instead of ++?
  //
  def doGuesses(cellNumber: Int, digits: Iterable[Int])
    : Iterable[Solution] =
  {
    digits.foldLeft(Stream.empty) {(accum, digit) =>
      val next = Next("Guess", digit, cellNumber)
      // xxx need the lazy magic here
      accum #::: placeAndContinue(next)
    }
  }
}

object Solver {
  def randomSolutions(
    options: SolverOptions,
    rnd: Random,
    puzzle: Puzzle)
    : Iterable[Solution] =
  {
    val solver = Solver(options, Some(rnd), puzzle)
    solver.solutionsTop
  }
}

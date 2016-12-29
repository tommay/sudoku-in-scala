package net.tommay.sudoku

case class Solver (
  val options: SolverOptions,
  val rnd: Option[Random],
  val puzzle: Puzzle,
  val unknowns: List[Unknown] = List(),
  val steps: List[Step] = List())	// xxx need initial puzzle step
{
  def place(cellNumber: Int, digit: Int) : Solver = {
    val newPuzzle = puzzle.place(cellNumber, digit)
    val newUnknowns = unknowns
      .filter(_.cellNumber != cellNumber)
      .map(_.place(cellNumber, digit))
    this.copy(puzzle = newPuzzle, unknowns = newUnknowns)
  }

  def solutionsTop : Stream[Solution] = {
    unknowns match {
      case Nil =>
	// No more unknowns, solved!
	Stream(Solution(puzzle, steps))
      case _ =>
	// Carry on with solutionsHeuristic
	solutionsHeuristic
    }
  }

  def solutionsHeuristic : Stream[Solution] = {
    // XXX
    solutionsStuck
  }

  def placeAndContinue(next: Next) : Stream[Solution] = {
    val placement = next.placement
    val newSolver = place(placement.cellNumber, placement.digit)
    val step = Step(newSolver.puzzle, Some(placement), next.description)
    // xxx use a Stream?  Prepend and reverse if/when needed?
    val newSteps = steps ++ List(step)
    val newSolver2 = newSolver.copy(steps = newSteps)
    newSolver2.solutionsTop
  }

  def solutionsStuck : Stream[Solution] = {
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
          val next = Next("Forced guess", Placement(digit, cellNumber))
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
              val shuffledPossible = Solver.maybeShuffle(rnd, possible)
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
    : Stream[Solution] =
  {
    digits.foldLeft(Stream.empty[Solution]) {(accum, digit) =>
      val next = Next("Guess", Placement(digit, cellNumber))
      // xxx need the lazy magic here
      accum #::: placeAndContinue(next)
    }
  }

  def applyOneTrickySetIfAllowed : Option[Solver] = {
    if (options.usePermanentTrickySets) {
      None // XXX
    }
    else {
      None
    }
  }
}

object Solver {
  def randomSolutions(
    options: SolverOptions,
    rnd: Random,
    puzzle: Puzzle)
    : Stream[Solution] =
  {
    val solver = Solver(options, Some(rnd), puzzle)
    solver.solutionsTop
  }

  def maybeShuffle[T](rnd: Option[Random], list: List[T]) : List[T] = {
    rnd match {
      case Some(rnd) => Util.shuffle(list, rnd)
      case _ => list
    }
  }
}

package net.tommay.sudoku

import net.tommay.sudoku.Heuristic._

case class Solver (
  val options: SolverOptions,
  val rnd: Option[Random],
  val puzzle: Puzzle,
  val unknowns: List[Unknown],
  val steps: List[Step],
  val heuristics: Iterable[Solver => Stream[Next]])
{
  def place(cellNumber: Int, digit: Int) : Solver = {
    val newPuzzle = puzzle.place(cellNumber, digit)
    val newUnknowns = unknowns
      .filter(_.cellNumber != cellNumber)
      .map(_.place(cellNumber, digit))
    this.copy(puzzle = newPuzzle, unknowns = newUnknowns)
  }

  // All of the calls in the solutions chain which eventually may come
  // back around to top are tail calls, but scala doesn't do tail call
  // optimization except for direct recursion.  But we should make
  // at most 81 deep nested calls to solutionsTop to solve a puzzle so
  // we shouldn't blow the stack.  If we do, there are always trampolines:
  // http://stackoverflow.com/questions/16539488/why-scala-doesnt-make-tail-call-optimization
  // scala.util.control.TailCalls
  // 
  def solutionsTop : Stream[Solution] = {
    unknowns match {
      case Nil =>
	// No more unknowns, solved!
	Stream(Solution(puzzle, steps.reverse))
      case _ =>
	// Carry on with solutionsHeuristic
	solutionsHeuristic
    }
  }

  def solutionsHeuristic : Stream[Solution] = {
    if (options.useHeuristics) {
      // Try the heuristic functions.
      tryHeuristics(heuristics) match {
	case Stream.Empty =>
	  // All heuristics returned empty lists.
	  solutionsStuck
	case nextList =>
	  val (rnd1, rnd2) = Solver.maybeSplit(rnd)
	  // XXX if we're not doing things at random then all we ever need
	  // is the first element.
	  val next = Solver.pickRandom(nextList, rnd1)
	  val nextSolver = this.copy(rnd = rnd2)
	  nextSolver.placeAndContinue(next)
      }
    }
    else {
      // Skip the heuristics and continue with solutionsStuck.
      solutionsStuck
    }
  }

  // Call each function on this, and return the first non-empty
  // result.  Return an empty list if all results are empty.

  def tryHeuristics(list: Iterable[Solver => Stream[Next]])
    : Stream[Next] =
  {
    list match {
      case Nil => Stream.empty
      case func :: tail =>
	func(this) match {
	  case Stream.Empty => tryHeuristics(tail)
	  case nextList => nextList
	}
    }
  }

  def placeAndContinue(next: Next) : Stream[Solution] = {
    val placement = next.placement
    val newSolver = place(placement.cellNumber, placement.digit)
    val step = Step(newSolver.puzzle, Some(placement), next.description)
    // xxx use a Stream?  Prepend and reverse if/when needed?
    val newSteps = step :: steps
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
          val next = Next("Forced guess", Placement(cellNumber, digit))
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
	// possibiities for heuristics.
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

  // XXX On Haskell this works brilliantly, and solutions are
  // calcuated lazily while Solve is printing and counting them, and a
  // small fixed amount of memory is used no matter how many solutions
  // there are.  On Scala the calculation and printing are correctly
  // interleaved, but memory use increases and eventually blows up if
  // there are thousands of solutions.  It's like something is holding
  // ontp a head somewhere.  Something's not right.

  // XXX Actually this is not the same as Haskell because the Haskell
  // code is passing in previous solutions tail-call style.
  
  def doGuesses(cellNumber: Int, digits: Iterable[Int])
    : Stream[Solution] =
  {
    digits.foldLeft(Stream.empty[Solution]) {(accum, digit) =>
      val next = Next("Guess", Placement(cellNumber, digit))
      accum #::: placeAndContinue(next)
    }
  }

  def findForced : Stream[Next] = {
    // XXX Should unknowns be a Stream to begin with?
    unknowns.toStream.flatMap(findForcedForUnknown("Forced"))
  }

  // This can return either List or Stream.  But since it's going to be
  // flatMap'd by a Stream, returning Stream performs better.

  def findForcedForUnknown(description: String)(unknown: Unknown) :
    Stream[Next] =
  {
    unknown.getPossible match {
      case List(digit) =>
	Stream(Next(description, Placement(unknown.cellNumber, digit)))
      case _ => Stream.empty
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

  def findEasyPeasy : Stream[Next] = {
    Stream.empty
  }

  def findMissingOne : Stream[Next] = {
    Stream.empty
  }

  def findMissingTwo : Stream[Next] = {
    Stream.empty
  }

  def findTricky : Stream[Next] = {
    Stream.empty
  }

  def findNeeded : Stream[Next] = {
    Stream.empty
  }
}

object Solver {
  def empty(options: SolverOptions, rnd: Option[Random], puzzle: Puzzle)
    : Solver =
  {
    val (rnd1, rnd2) = maybeSplit(rnd)
    val unknowns = maybeShuffle(rnd1, (0 to 80).map(Unknown(_)).toList)
    val step = Step(puzzle, None, "Initial puzzle")
    val heuristicFunctions = options.heuristics.map(getHeuristicFunction)
    new Solver(options, rnd, puzzle, unknowns, List(step),
	       heuristicFunctions)
  }

  def create(options: SolverOptions, rnd: Option[Random], puzzle: Puzzle)
    : Solver =
  {
    val solver = empty(options, rnd, puzzle)
    puzzle.each.foldLeft(solver) {case (accum, (cellNumber, digit)) =>
      accum.place(cellNumber, digit)
    }
  }

  // Heuristic functions return a Stream.  We may need only the first
  // result (when creating a Puzzle, to see whether the Puzzle is
  // solvable), or we may pick a random result (when providing a
  // hint).  Using a Stream makes it ok either way since we'll only
  // compute what we need.

  def getHeuristicFunction(heuristic: Heuristic) : Solver => Stream[Next] = {
    heuristic match {
      case EasyPeasy => {_.findEasyPeasy}
      case MissingOne => {_.findMissingOne}
      case MissingTwo => {_.findMissingTwo}
      case Tricky => {_.findTricky}
      case Needed => {_.findNeeded}
      case Forced => {_.findForced}
    }
  }

  def randomSolutions(
    options: SolverOptions,
    rnd: Random,
    puzzle: Puzzle)
    : Stream[Solution] =
  {
    val solver = Solver.create(options, Some(rnd), puzzle)
    solver.solutionsTop
  }

  def maybeSplit(rnd: Option[Random]) : (Option[Random], Option[Random]) = {
    rnd match {
      case r@Some(rnd) => (r, Some(rnd.fork))
      case _ => (rnd, rnd)
    }
  }

  def maybeShuffle[T](rnd: Option[Random], list: List[T]) : List[T] = {
    rnd match {
      case Some(rnd) => Util.shuffle(list, rnd)
      case _ => list
    }
  }

  def pickRandom[T](list: Iterable[T], rnd: Option[Random]) : T = {
    // XXX
    list.head
  }
}

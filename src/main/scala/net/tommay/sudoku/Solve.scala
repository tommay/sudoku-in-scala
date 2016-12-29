package net.tommay.sudoku

import net.tommay.sudoku.Heuristic._

object Solve {
  // Initializes Puzzle from the given Filename and prints out solutions
  // if any.

  def main(args: Array[String]) {
    val filename = args(1)
    val setup = getSetup(filename)
    val puzzle = Puzzle.fromString(setup)
    val rnd = new scala.util.Random(0)
    val solutions = Solver.randomSolutions(options, rnd, puzzle)
    val count = processAndCount(solutions, printSolution)
    println(s"There are $count solutions.")
  }

  val heuristics = List(
    EasyPeasy,
    MissingOne,
    MissingTwo,
    Needed,
    Forced,
    Tricky)

  val options = new SolverOptions(
    heuristics = List(),
    usePermanentTrickySets = false,
    useGuessing = true)

  def processAndCount[T](list: Iterable[T], func: T => Unit, count: Int = 0)
    : Int =
  {
    list.headOption match {
      case Some(head) =>
	func(head)
        processAndCount(list.tail, func, count + 1)
      case _ => count
    }
  }

  def printSolution(solution: Solution) {
    if (true) {
      solution.steps.foreach {step =>
        println(showStep(step))
      }
      println(solution.puzzle.toPuzzleString)
    }
  }

  def showStep(step: Step) : String = {
    step.description + (step.placementOption match {
      case Some(placement) =>
	val (row, col) = rowcol(placement.cellNumber)
	": ($row, $col) ${placement.digit}"
      case None => ""
    })
  }

  def rowcol(n: Int) = {
    (n / 9, n % 9)
  }

  // Returns the contents of filename as a string with "#" comments
  // and whitespace deleted.  The result should be a string of 81
  // digits or dashes, where the digits are given by the puzzle and
  // the dash cells are to be solved for.

  def getSetup(filename: String) = {
    val raw = scala.io.Source.fromFile(filename).mkString
    val noComments = "#.*".r.replaceAllIn(raw, "")
    val setup = """\s+""".r.replaceAllIn(noComments, "")
    setup
  }
}

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
	List(Solution(puzzle, steps))
      case _ =>
	// Carry on with solutionsHeuristic
	solutionsHeuristic
    }
  }

  def solutionsHeuristic : Iterable[Solution] = {
    List()
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

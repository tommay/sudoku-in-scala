package net.tommay.sudoku

case class Solver (
  val options: SolverOptions,
  val puzzle: Puzzle,
  val rnd: Option[scala.util.Random],
  val unknowns: List[Unknown] = List(),
  val steps: List[Step] = List()	// xxx need initial puzzle step
{
  def solutionsTop : Iterable[Solution] = {
    unknowns match {
      case Nil =>
	// No more unknowns, solved!
	List(Solution(puzzle, steps))
      _ => solutionsHeuristic
    }
  }
}

object Solver {
  def randomSolutions(
    options: SolverOptions,
    rnd: scala.util.Random,
    puzzle: Puzzle) : Iterable[Solutions] =
  {
    val solver = Solver(options, Some(rnd), puzzle)
    solver.solutionsTop
  }
}

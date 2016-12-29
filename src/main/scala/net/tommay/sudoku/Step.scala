package net.tommay.sudoku

case class Step(
  val puzzle: Puzzle,
  val placementOption: Option[Placement],
  val description: String)
{
}

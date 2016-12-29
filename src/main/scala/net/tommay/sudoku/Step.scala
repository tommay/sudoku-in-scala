package net.tommay.sudoku

case class Step(
  val description: String,
  val puzzle: Puzzle,
  val placementOption: Option[Placement])
{
}

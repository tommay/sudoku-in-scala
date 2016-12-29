package net.tommay.sudoku

case class Solution(
  val puzzle: Puzzle,
  val steps: Iterable[Step])
{
}

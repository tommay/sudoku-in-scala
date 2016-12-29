package net.tommay.sudoku

object Heuristic extends Enumeration
{
  type Heuristic = Value
  val
    EasyPeasy,
    MissingOne,
    MissingTwo,
    Needed,
    Forced,
    Tricky
  = Value
}

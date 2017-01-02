package  net.tommay.sudoku

case class Unknown(
  cellNumber: Int,
  row: Int,
  col: Int,
  square: Int,
  possible: Int)
{
  def place(cellNumber: Int, digit: Int) : Unknown = {
    // Before bothering to test isExcludedBy, check whether the digit
    // has already been removed.  This is just an optimization but
    // makes a big difference in Frege.
    if (isDigitPossible(digit)) {
      val other = Unknown(cellNumber)
      if (isExcludedBy(other)) {
        removeDigitFromPossible(digit)
      }
      else {
        this
      }
    }
    else {
      this
    }
  }

  def isDigitPossible(digit: Int) : Boolean = {
    (possible & (1 << (digit - 1))) != 0
  }

  def removeDigitFromPossible(digit: Int) : Unknown = {
    this.copy(possible = possible & ~(1 << (digit - 1)))
  }

  def numPossible : Int = {
    Integer.bitCount(possible)
  }

  def getPossible : List[Int] = {
    Unknown.possibleLists(possible)
  }

  // Returns true if this and Other are in the same row, column, or
  // square, else false.
  // An Unknown does not exclude itself.  I'm not sure we actually
  // have to check for this in practice, but better safe than sorry.

  def isExcludedBy(other: Unknown) : Boolean = {
    (this.cellNumber != other.cellNumber) &&
    (this.row == other.row ||
     this.col == other.col ||
     this.square == other.square)
  }

// Check for equality by testing cellNumber and possible.  The other fields
// are functions of cellNumber.
//
/*
instance Eq Unknown where
  this == that = 
    (Unknown.cellNumber this == Unknown.cellNumber that) &&
    (Unknown.possible this == Unknown.possible that)
  hashCode this = (this.cellNumber * 23) + this.possible.hashCode
*/
}

object Unknown {
  val possibleLists = (0 to 511).map(makePossibleList(_)).toArray

  // Returns a new Unknown at position cellNumber.  Determine the
  // Unknown's row, column, and square, set all digits possible.

  def apply(cellNumber: Int) : Unknown = {
    val row = cellNumber / 9
    val col = cellNumber % 9
    val square = (row / 3)*3 + (col / 3)
    new Unknown(
      cellNumber = cellNumber,
      row = row,
      col = col,
      square = square,
      possible = 0x1FF)
  }

  def makePossibleList(possible: Int, digit: Int = 1) : List[Int] = {
    if (possible == 0) {
      List()
    }
    else {
      val rest = makePossibleList(possible >> 1, digit + 1)
      if ((possible & 1) != 0) {
        digit :: rest
      }
      else {
        rest
      }
    }
  }
}

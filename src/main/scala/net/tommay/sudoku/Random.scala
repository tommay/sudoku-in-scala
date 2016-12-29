package net.tommay.sudoku

// XXX stub class

case class Random(seed: Int) {
  def this() = {
    this(0)
  }

  def fork : Random = {
    Random(next ^ 0x87654321)
  }

  def randomR(lo: Int, hi: Int) : (Int, Random) = {
    val r = next
    val result = r % (hi - lo) + lo
    (result, Random(r))
  }

  def next : Int = {
    1
  }
}

object Random {
}


package net.tommay.sudoku

// XXX stub class

case class Random(seed: Int) {
  def this() = {
    // 2 is a random number.
    this(2)
  }

  def next : Int = {
    seed
  }

  def randomR(lo: Int, hi: Int) : (Int, Random) = {
    val r = next
    val result = r % (hi - lo) + lo
    (result, Random(r))
  }

  def fork : Random = {
    new Random()
  }
}

object Random {
}

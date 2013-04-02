import org.scalatest.FunSuite

import org.scalatest.junit.JUnitRunner

class AISuite extends FunSuite {
  import Game._

  test("search: should return move (0, 0)") {
    val repr = Array.ofDim[Int](3,3)
    repr(0)(1) = 1
    repr(0)(2) = 1
    repr(1)(1) = 2
    repr(1)(2) = 2
    val board = new Board(repr)
    assert(AI.search(board, new Computer) === (0, 0))
  }
}

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class AISuite extends FunSuite with BeforeAndAfter {
  import Game._

  var repr: Array[Array[Int]] = _

  before {
    repr = Array.ofDim[Int](3,3)
  }

  // Computer should block winning move
  test("search: should return move (0, 0)") {
    repr(0)(1) = 1
    repr(0)(2) = 1
    repr(1)(1) = 2
    val board = new Board(repr)
    assert(AI.search(board, new Computer) === (0, 0))
  }

  // Starting from a corner, the Computer should always choose the center to not lose eventually
  test("search: should return move (1, 1)") {
    repr(0)(0) = 1
    val board = new Board(repr)
    assert(AI.search(board, new Computer) === (1, 1))
  }

  // When CPU starts, it should always choose a corner which maximizes its chance to win
  test("search: should return a corner move") {
    val board = new Board(repr)
    val move = AI.search(board, new Computer)
    val check = move == (0, 0) || move == (0, 2) || move == (2, 0) || move == (2, 2)
    assert(check, s"CPU did not choose corner move, it chose ${move}")
  }
}

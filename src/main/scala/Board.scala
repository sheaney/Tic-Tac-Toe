import annotation.switch

class Board(private val repr: Array[Array[Int]] = Array.fill(3,3)(0)) extends Utilities {

  def update(player: Player) {
    var i, j = 0
    val (row, column) = player.getMove
    player match {
      case _: Human => repr(row)(column) = 1
      case _: Computer => repr(row)(column) = 2
    }
  }

  def simulate(move: Move, player: Player): Board = {
    val board = this.copy
    var (i, j) = move
    player match {
      case _: Human => board.repr(i)(j) = 1
      case _: Computer => board.repr(i)(j) = 2
    }
    board
  }

  def findPossibleMoves: Stream[Move] = {
    for {
      i <- (0 to 2).toStream
      j <- (0 to 2).toStream
      if repr(i)(j) == 0
    } yield (i, j)
  }

  def fitness: Int = {
    checkForWinner match {
      case Some(_: Human) => -1
      case Some(_: Computer) => 1
      case _ => 0
    }
  }

  def getGameState: GameState = {
    if (checkForWinner.isEmpty)
      checkIfBoardIsFull
    else
      Winner()
  }

  def checkForWinner: Option[Player] = {
    checkForVerticalWinner
      .orElse(checkForHorizontalWinner)
      .orElse(checkForDiagonalWinner)
  }

  def checkForVerticalWinner: Option[Player] = {
    var j = 0
    while (j < 3) {
      var i = 0
      var winner = true
      val check = repr(i)(j)
      while (i < 3) {
        if (repr(i)(j) == 0 || check != repr(i)(j))
          winner = false
        i += 1
      }
      if (winner) {
        return getWinner(check)
      }
      j += 1
    }
    return None
  }

  def checkForHorizontalWinner: Option[Player] = {
    var i = 0
    while (i < 3) {
      var j = 0
      var winner = true
      val check = repr(i)(j)
      while (j < 3) {
        if (repr(i)(j) == 0 || check != repr(i)(j))
          winner = false
        j += 1
      }
      if (winner) {
        return getWinner(check)
      }
      i += 1
    }
    return None
  }

  def checkForDiagonalWinner: Option[Player] = {
    var i = 0
    val check = repr(i)(i)
    var winner = true
    while (i < 3) { // main diagonal
      if (repr(i)(i) == 0 || check != repr(i)(i))
        winner = false
      i += 1
    }
    if (winner) {
      return getWinner(check)
    }
    i = 0
    var j = 2
    val check2 = repr(i)(j)
    winner = true
    while (i < 3) { // inverse diagonal
      if (repr(i)(j) == 0 || check2 != repr(i)(j))
        winner = false
      i += 1
      j -= 1
    }
    if (winner) {
      return getWinner(check2)
    }
    return None
  }

  def checkIfBoardIsFull: GameState = {
    var i = 0
    while (i < 3) {
      var j = 0
      while (j < 3) {
        if (repr(i)(j) == 0) 
          return Running()
        j += 1
      }
      i += 1
    }
    return Tied()
  }

  private def getWinner(winner: Int): Option[Player] =
    (winner: @switch) match {
      case 1 => return Some(new Human())
      case 2 => return Some(new Computer())
    }

  def getPlayer(i: Int, j: Int): Int = this.repr(i)(j)

  def printBoard: Unit = println(this)

  def copy = new Board(repr = repr.map(_.clone))

  override def toString = strBoard

	private def strBoard: String = {
    def makeRow(row: Array[Int]): String = {
      val disksInRow = 
        for (cell <- row) yield {
          cell match {
            case 0 => " - "
            case 1 => " X "
            case 2 => " O "
          }
        }
      disksInRow.mkString 
    }

    val upperRow = (0 until repr.length).toList mkString ("   ", "  ", "\n")
    val bottomRows = 
      for ((row, i) <- repr.zipWithIndex)
      yield i +" "+ makeRow(row)
    upperRow + bottomRows.mkString("\n")
  }
}

object GameBoard extends Board

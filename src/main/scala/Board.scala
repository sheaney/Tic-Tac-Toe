import annotation.switch

class Board(protected val repr: Array[Array[Int]] = Array.fill(3,3)(0)) extends Utilities {

  def update(player: Player, players: Players) {
    val (row, column) = player.getMove(players)
    player match {
      case _: Player1 => repr(row)(column) = 1
      case _: Player2 => repr(row)(column) = 2
    }
  }

  def simulate(move: Move, player: Player): Board = {
    val board = this.copy
    val (row, column) = move
    player match {
      case _: Player1 => board.repr(row)(column) = 1
      case _: Player2 => board.repr(row)(column) = 2
    }
    board
  }

  def findPossibleMoves: Stream[Move] =
    for {
      row <- (0 to 2).toStream
      column <- (0 to 2).toStream
      if repr(row)(column) == 0
    } yield (row, column)

  def fitness(players: Players): Int =
    checkForWinner(players) match {
      case Some(_: Human) => -1
      case Some(_: Computer) => 1
      case _ => 0
    }

  def getGameState(players: Players): GameState =
    if (checkForWinner(players).isEmpty)
      checkIfBoardIsFull
    else
      Winner()

  def checkForWinner(players: Players): Option[Player] =
    checkForVerticalWinner(players)
      .orElse(checkForHorizontalWinner(players))
      .orElse(checkForDiagonalWinner(players))

  def checkForVerticalWinner(players: Players): Option[Player] = {
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
        return getWinner(check, players)
      }
      j += 1
    }
    None
  }

  def checkForHorizontalWinner(players: Players): Option[Player] = {
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
        return getWinner(check, players)
      }
      i += 1
    }
    None
  }

  def checkForDiagonalWinner(players: Players): Option[Player] = {
    var i = 0
    val check = repr(i)(i)
    var winner = true
    while (i < 3) { // main diagonal
      if (repr(i)(i) == 0 || check != repr(i)(i))
        winner = false
      i += 1
    }
    if (winner) {
      return getWinner(check, players)
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
      return getWinner(check2, players)
    }
    None
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
    Tied()
  }

  private def getWinner(winner: Int, players: Players): Option[Player] =
    (winner: @switch) match {
      case 1 => Some(players._1)
      case 2 => Some(players._2)
    }

  def getPlayer(i: Int, j: Int): Int = this.repr(i)(j)

  def printBoard: Unit = println(this)

  def copy = new Board(repr = repr.map(_.clone))

  override def toString = strBoard

	private def strBoard: String = {
    def makeRow(row: Array[Int]): String = {
      val marksInRow =
        for (cell <- row) yield {
          cell match {
            case 0 => " - "
            case 1 => " X "
            case 2 => " O "
          }
        }
      marksInRow.mkString 
    }

    val upperRow = (0 until repr.length).toList mkString ("   ", "  ", "\n")
    val bottomRows = 
      for ((row, i) <- repr.zipWithIndex)
      yield i +" "+ makeRow(row)
    upperRow + bottomRows.mkString("\n")
  }
}

object GameBoard extends Board

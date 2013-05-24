import annotation.switch

class Board(private val repr: Array[Array[Int]] = Array.fill(3,3)(0)) extends Utilities {

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
    getWinner(players) match {
      case Some(_: Human) => -1
      case Some(_: Computer) => 1
      case _ => 0
    }

  def getGameState(players: Players): GameState =
    if (getWinner(players).isEmpty)
      checkIfBoardIsFull
    else
      Winner()

  def getWinner(players: Players): Option[Player] =
    getVerticalWinner(players)
      .orElse(getHorizontalWinner(players))
      .orElse(getDiagonalWinner(players))

  def getVerticalWinner(players: Players): Option[Player] = {
    val indeces = Seq(0, 1, 2)
    def checkColumn(col: Int): Boolean = {
      val check = repr(indeces.head)(col)
      indeces.forall { row =>
        repr(row)(col) == check
      }
    }

    getWinner(
      indeces.foldLeft(0) { (winner, col) =>
        if (winner != 0) winner
        else if (checkColumn(col)) repr(col)(col)
        else 0
      },
      players
    )
  }

  def getHorizontalWinner(players: Players): Option[Player] =
    getWinner(
      repr.foldLeft(0) { (winner, row) =>
        if (winner != 0) winner
        else if (row.forall(_ == row.head)) row.head
        else 0
      },
      players
    )

  def getDiagonalWinner(players: Players): Option[Player] = {
    val indeces = List(0, 1, 2)
    if (regularDiagonal(indeces))
      getWinner(repr(indeces.head)(indeces.head), players)
    else if (reverseDiagonal(indeces))
      getWinner(repr(1)(1), players)
    else
      getWinner(0, players)
  }

  private def regularDiagonal(indeces: Seq[Int]): Boolean =
    indeces.forall(index => repr(index)(index) == repr(indeces.head)(indeces.head))

  private def reverseDiagonal(indeces: Seq[Int]): Boolean =
    indeces.reverse.zip(indeces) forall { case (row, col) =>
      repr(row)(col) == repr(1)(1)
    }

  def checkIfBoardIsFull: GameState = {
    def isFull: Boolean =
      repr.foldLeft(true) { (isFull, row) =>
        isFull && row.forall(_ != 0)
      }
    if (isFull) Tied() else Running()
  }

  private def getWinner(winner: Int, players: Players): Option[Player] =
    (winner: @switch) match {
      case 0 => None
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

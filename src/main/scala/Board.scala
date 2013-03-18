import annotation.switch

class Board(val repr: Array[Array[Int]] = Array.fill(3,3)(0)) extends Utilities {
  var p1FreeRows, p2FreeRows, p1FreeCols, p2FreeCols, p1FreeDiag, p2FreeDiag = 0
  var i, j = 0
  var free = true
  var x, y = false

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

  def fitness2: Int = {
    checkForWinner match {
      case Some(_: Human) => -1
      case Some(_: Computer) => 1
      case _ => 0
    }
  }

  def fitness: Int = {
    val (p1FreeRows, p2FreeRows) = freeRows
    val (p1FreeCols, p2FreeCols) = freeCols
    val (p1FreeDiag, p2FreeDiag) = freeDiag
    (p2FreeRows + p2FreeCols + p2FreeDiag) -
    (p1FreeRows + p1FreeCols + p1FreeDiag)
  }

  private def updateParams() {
    if (repr(i)(j) != 0) {
      free = false
      if (repr(i)(j) == 1)
        x = true
      else
        y = true
    }
  }

  private def updateCounters(check: Int) {
    (check: @switch) match {
      case 1 =>
        if (free) {
          p1FreeRows += 1
          p2FreeRows += 1
        } else if (x && !y) {
            p1FreeRows += 1
        } else if (!x && y) {
            p2FreeRows += 1
        }
      case 2 =>
        if (free) {
          p1FreeCols += 1
          p2FreeCols += 1
        } else if (x && !y) {
            p1FreeCols += 1
        } else if (!x && y) {
            p2FreeCols += 1
        }
      case 3 =>
        if (free) {
          p1FreeDiag += 1
          p2FreeDiag += 1
        } else if (x && !y) {
            p1FreeDiag += 1
        } else if (!x && y) {
            p2FreeDiag += 1
        }
    }
  }

  def freeRows: (Int, Int) = {
    p1FreeRows = 0; p2FreeRows= 0
    i = 0
    while (i < 3) {
      j = 0
      free = true
      x = false; y = false
      while (j < 3) {
        updateParams()
        j += 1
      }
      updateCounters(1)
      i += 1
    }

    (p1FreeRows, p2FreeRows)
  }

  def freeCols: (Int, Int) = {
    p1FreeCols = 0; p2FreeCols = 0
    j = 0
    while (j < 3) {
      i = 0
      free = true
      x = false; y = false
      while (i < 3) {
        updateParams()
        i += 1
      }
      updateCounters(2)
      j += 1
    }

    (p1FreeCols, p2FreeCols)
  }

  def freeDiag: (Int, Int) = {
    p1FreeDiag = 0; p2FreeDiag = 0
    i = 0; j = 0
    free = true
    x = false; y = false
    while (i < 3) { // main diagonal
      updateParams()
      i += 1
      j += 1
    }
    updateCounters(3)

    i = 0; j = 2
    free = true
    x = false; y = false
    while (i < 3) { // inverse diagonal
      updateParams()
      i += 1
      j -= 1
    }
    updateCounters(3)

    (p1FreeDiag, p2FreeDiag)
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

object GameBoard extends Board {
  /*repr(0)(0) = 1
  repr(1)(0) = 2
  repr(1)(1) = 2
  repr(1)(2) = 1*/
}

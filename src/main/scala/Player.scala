trait Player extends Utilities {
  def getMove: Move
}

class Human extends Player {
  def getMove: Move = {
    println("Enter i: ")
    val i = readInt
    println("Enter j: ")
    val j = readInt
    (i, j)
  }
}

class Computer extends Player {
  val r = scala.util.Random
  def getMove: Move = {
    AI.search(GameBoard.copy, this)
  }
}

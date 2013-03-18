trait Player extends Utilities {
  def getMove: Move
}

class Human extends Player {
  def getMove: Move = GUI.awaitMoveSelection
}

class Computer extends Player {
  def getMove: Move = AI.search(GameBoard.copy, this)
}

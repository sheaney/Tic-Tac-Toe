trait Player extends Utilities {
  def getMove(players: Players): Move
}

class Player1 extends Player {
  // mock method that will be overidden
  def getMove(players: Players): Move = (0, 0)
}

class Player2 extends Player {
  // mock method that will be overidden
  def getMove(players: Players): Move = (0, 0)
}

trait Human extends Player {
  override def getMove(players: Players): Move = GUI.awaitMoveSelection
}

trait Computer extends Player {
  override def getMove(players: Players): Move = AI.search(GameBoard.copy, this, players)
}

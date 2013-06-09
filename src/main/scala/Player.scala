trait Player extends Utilities {
  def getMove(implicit players: Players): Move
}

trait Player1 extends Player

trait Player2 extends Player

trait Human extends Player {
  def getMove(implicit players: Players): Move = GUI.awaitMoveSelection
}

trait Computer extends Player {
  def getMove(implicit players: Players): Move = AI.search(GameBoard.copy, this)
}

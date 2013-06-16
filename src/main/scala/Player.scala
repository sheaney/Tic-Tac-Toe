trait Player extends Utilities

trait Player1 extends Player

trait Player2 extends Player

trait Human extends Player

trait Computer extends Player {
  def getMove(implicit players: Players): Move = AI.search(GameBoard.copy, this)
}

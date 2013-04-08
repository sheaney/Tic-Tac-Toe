sealed abstract class GameState
case class Running() extends GameState

abstract class GameOver() extends GameState
case class Winner() extends GameOver
case class Tied() extends GameOver

object Game extends Utilities {

  def main(args: Array[String]) {
    (initialize andThen run)(args)
  }

  def initialize = (args: Array[String]) => {
      GUI.start
      args.map(_.trim) match {
        case Array("1") =>
          (new Player1 with Human, new Player2 with Computer)
        case Array("2") =>
          (new Player1 with Computer, new Player2 with Human)
        case _ =>
          (new Player1 with Human, new Player2 with Computer)
        }
    }

  def run(players: Players) {
    GUI.setWinner(gameLoop(players._1, players, Running()))
  }

  def gameLoop(player: Player, players: Players, gameState: GameState): Option[Player] =
    gameState match {
      case _: Running =>
        nextTurn(player, players)
        gameLoop(nextPlayer(player, players), players, GameBoard.getGameState(players))
      case _: GameOver =>
        GameBoard.checkForWinner(players)
    }

  def nextTurn(player: Player, players: Players) {
    GUI.update(player)
    GameBoard.update(player, players)
  }

  def nextPlayer(player: Player, players: Players) = player match {
    case _: Player1 => players._2
    case _: Player2 => players._1
  }

}

sealed trait GameState
case object Running extends GameState

sealed trait GameOver extends GameState
case object Winner extends GameOver
case object Tied extends GameOver

object Game extends Utilities {

  def main(args: Array[String]) {
    (initialize andThen run)(args)
  }

  def initialize = (args: Array[String]) => {
      GUI.start
      args.map(_.trim.toLowerCase) match {
        case Array("x") =>
          (new Player1 with Human, new Player2 with Computer)
        case Array("o") =>
          (new Player1 with Computer, new Player2 with Human)
        case _ =>
          (new Player1 with Human, new Player2 with Computer)
        }
    }

  def run(players: Players) {
    GUI.setWinner(gameLoop(players._1, players, Running))
  }

  def gameLoop(player: Player, players: Players, gameState: GameState): Option[Player] =
    gameState match {
      case Running =>
        nextTurn(player, players)
        gameLoop(nextPlayer(player, players), players, GameBoard.getGameState(players))
      case _: GameOver =>
        GameBoard.getWinner(players)
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

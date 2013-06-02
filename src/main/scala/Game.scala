sealed trait GameState
case object Running extends GameState

sealed trait GameOver extends GameState
case object Winner extends GameOver
case object Tied extends GameOver

object Game extends Utilities {

  def main(args: Array[String]) {
    (initialize andThen run)(args)
  }

  lazy val initialize = (args: Array[String]) => {
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
    GUI.setWinner(gameLoop(players._1, Running)(players))
  }

  def gameLoop(player: Player, gameState: GameState)(implicit players: Players): Option[Player] =
    gameState match {
      case Running =>
        nextTurn(player)
        gameLoop(nextPlayer(player), GameBoard.getGameState)
      case _: GameOver =>
        GameBoard.getWinner
    }

  def nextTurn(player: Player)(implicit players: Players) {
    GUI.update(player)
    GameBoard.update(player)
  }

  def nextPlayer(player: Player)(implicit players: Players) = player match {
    case _: Player1 => players._2
    case _: Player2 => players._1
  }

}

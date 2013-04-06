sealed abstract class GameState
case class Running() extends GameState

abstract class GameOver() extends GameState
case class Winner() extends GameOver
case class Tied() extends GameOver

object Game extends Utilities {

  type Players = Tuple2[Player, Player]

  def main(args: Array[String]) {
    (initialize andThen run)(args)
  }

  def initialize = (args: Array[String]) => { GUI.start; (new Human, new Computer) }

  def run(players: Players) {
    GUI.setWinner(gameLoop(players._1, players, Running()))
  }

  def gameLoop(player: Player, players: Players, gameState: GameState): Option[Player] =
    gameState match {
      case _: Running =>
        nextTurn(player)
        gameLoop(nextPlayer(player, players), players, GameBoard.getGameState)
      case _: GameOver =>
        GameBoard.checkForWinner
    }

  def nextTurn(player: Player) {
    GUI.update(player)
    GameBoard.update(player)
  }

  def nextPlayer(player: Player, players: Players) = player match {
    case _: Human => players._2
    case _: Computer => players._1
  }
}

trait GameState
case class Running() extends GameState
case class Winner() extends GameState
case class Tied() extends GameState

object Game extends Utilities {
  def main(args: Array[String]) {
    (initialize andThen run)(args)
  }

  def initialize = (args: Array[String]) => (new Human, new Computer)

  def run(players: Players) {
    gameLoop(players._1, players, Running()) match {
      case Some(_: Human) =>
        println("You won!")
      case Some(_: Computer) =>
        println("Better luck next time!")
      case _ =>
        println("You tied")
    }
    GameBoard.printBoard
  }

  def gameLoop(player: Player, players: Players, gameState: GameState): Option[Player] =
    gameState match {
      case _: Running =>
        nextTurn(player)
        gameLoop(nextPlayer(player, players), players, GameBoard.getGameState)
      case _: Winner =>
        GameBoard.checkForWinner
      case _: Tied =>
        None
    }

  def nextTurn(player: Player) {
    GameBoard.update(player)
  }

  def nextPlayer(player: Player, players: Players) = player match {
    case _: Human => players._2
    case _: Computer => players._1
  }
}

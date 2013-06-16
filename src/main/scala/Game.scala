import akka.actor.{ActorSystem, Props}

sealed trait GameState
case object Running extends GameState

sealed trait GameOver extends GameState
case object Winner extends GameOver
case object Tied extends GameOver

object Game extends Utilities {

  def main(args: Array[String]): Unit =
    (initialize andThen run)(args)

  val initialize = (args: Array[String]) => {
    args.map(_.trim.toLowerCase) match {
      case Array("x") =>
        (new Player1 with Human, new Player2 with Computer)
      case Array("o") =>
        (new Player1 with Computer, new Player2 with Human)
      case _ =>
        (new Player1 with Human, new Player2 with Computer)
      }
    }

  def run(players: Players): Unit = {
    val system = ActorSystem("TicTacToe")
    system.actorOf(Props(classOf[GameActor], players), "GameActor")
  }

}

import akka.actor.{ActorSystem, ActorContext, Props, Actor}

// Game Actor Messages
case class TakeTurn(player: Human, move: (Int, Int))
case class SearchMove(player: Computer)
case class Update(player: Player)
case object Exit

class GameActor(implicit val players: Tuple2[Player1,Player2]) extends Actor with Utilities {
  // Indicate if player 1 is the user or the computer
  var currentTurn: Option[Player] =
    players match {
      case (h: Human, c: Computer) => Some(h)
      case (c: Computer, h: Human) => Some(c)
      case _ => None
    }

  // Create GUIActor
  val GuiActor = context.actorOf(Props(classOf[GUIActor], players), "GUIActor")
  initialize()

  private def initialize(): Unit = {
    GuiActor ! Running
    currentTurn match {
      case Some(c: Computer) =>
        context.self ! SearchMove(c)
      case _ =>
    }
  }

  def receive = {
    case TakeTurn(player, move) =>
      // If not my turn or not a legal move ignore GUIActor messages
      currentTurn.filter { current =>
        (player, current) match {
          case (_: Human, _: Human) if legalSelection(move) => false
          case _ => true
        }
      } getOrElse makeMove(player, move)
    case SearchMove(player) =>
      val move = player.getMove(players)
      makeMove(player, move)
    case Exit =>
      context.system.shutdown
  }

  private def legalSelection(move: Move): Boolean = {
    val (row, column) = move
    if (GameBoard.getPlayer(row, column) == 0) true
    else false
  }

  private def makeMove(player: Player, move: Move)(implicit players: Players): Unit = {
    GameBoard.update(player, move)
    // send GUIActor a message to update itself
    GuiActor ! Update(player)
    currentTurn = currentTurn.fold[Option[Player]](None)(liftNextPlayer)
  }

  /**
   * Lift the current player into next player
   */
  private def liftNextPlayer(player: Player)(implicit players: Players): Option[Player] =
    player match {
      case _: Player1 => Some(players._2)
      case _: Player2 => Some(players._1)
    }

  private def nextPlayer(player: Player)(implicit players: Players) = player match {
    case _: Player1 => players._2
    case _: Player2 => players._1
  }
}

import scala.swing._
import scala.swing.event.{TableRowsSelected, TableColumnsSelected}

import java.awt.{Dimension, Color}
import javax.swing.ImageIcon

import akka.actor.{ActorSystem, ActorContext, Props, Actor}

case class Selection(move: (Int, Int))

class GUIActor(implicit val players: Tuple2[Player1,Player2]) extends Actor with Publisher with Utilities {

  var currentPlayer: Player = players._1
  val x = new Label { icon = new ImageIcon("images/X.jpg") }
  val o = new Label { icon = new ImageIcon("images/O.jpg") }
  val empty = new Label("")

  val table = new Table(3, 3) {
    background = new Color(255, 255, 255)
    autoResizeMode = Table.AutoResizeMode.Off
    rowHeight = 100
    preferredSize = { new Dimension(300, 300) }
    gridColor = Color.BLACK
    selection.intervalMode = Table.IntervalMode.Single
    selection.elementMode = Table.ElementMode.Cell

    override def rendererComponent(isSelected: Boolean,
        hasFocus: Boolean, row: Int, column: Int): Component =
      renderCell(row, column)
  }

  val message = new Label("X's move")

  val mainFrame = new MainFrame {
    title = "Tic Tac Toe"
    location = new Point(200, 200)
    resizable = false
    message.horizontalTextPosition = Alignment.Center
    contents = new BoxPanel(Orientation.Vertical) {
      contents += message
      contents += table
    }

    listenTo(table.selection)

    reactions += {
      // User selected a cell to move to
      case TableRowsSelected(_, _, false) =>
        for (move <- table.selection.cells) {
          context.self ! Selection(move)
        }
      case TableColumnsSelected(_, _, false) =>
        for (move <- table.selection.cells) {
          context.self ! Selection(move)
        }
    }

    override def closeOperation(): Unit = {
      context.self ! Exit
      super.closeOperation()
    }
  }

  private def renderCell(row: Int, column: Int): Component =
    GameBoard.getPlayer(row, column) match {
      case 1 => x
      case 2 => o
      case _ => empty
    }

  private def setWinner(player: Option[Player]) {
    table.repaint
    player match {
      case Some(_: Player1) =>
        message.text = "X's won!"
      case Some(_: Player2) =>
        message.text = "O's won!"
      case _ =>
        message.text = "It's a tie!"
    }
  }

  private def update(player: Player) {
    table.repaint
    player match {
      case _: Player1 =>
        message.text = "X's move"
      case _: Player2 =>
        message.text = "O's move"
    }
  }

  private def nextPlayer(player: Player): Player =
    player match {
      case _: Player1 =>
        currentPlayer = players._2
        currentPlayer
      case _: Player2 =>
        currentPlayer = players._1
        currentPlayer
    }

  private def gameIsDone: Boolean = {
    GameBoard.getGameState match {
      case Running => false
      case _: GameOver => true
    }
  }

  // Message handler
  def receive = {
    case Update(player) =>
      nextPlayer(player)
      update(currentPlayer)
      if (!gameIsDone)
        currentPlayer match {
          case computer: Computer =>
            context.parent ! SearchMove(computer)
          case _ =>
        }
      else
        setWinner(GameBoard.getWinner)
    case Selection(move) =>
      currentPlayer match {
        case human: Human =>
          context.parent ! TakeTurn(human, move)
        case _ =>
      }
    case Running =>
      mainFrame.visible = true
    case Exit =>
      context.parent ! Exit
  }
}

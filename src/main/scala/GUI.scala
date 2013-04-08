import scala.swing._

import java.awt.{Dimension, Color}
import javax.swing.{ImageIcon, SwingUtilities}

object GUI {

  private val x = new Label { icon = new ImageIcon("images/X.jpg") }
  private val o = new Label { icon = new ImageIcon("images/O.jpg") }
  private val empty = new Label("")

  private val table = new Table(3, 3) {
    background = new Color(255, 255, 255)
    autoResizeMode = Table.AutoResizeMode.Off
    rowHeight = 100
    preferredSize = { new Dimension(300, 300) }
    gridColor = Color.BLACK
    selection.elementMode = Table.ElementMode.Cell

    override def rendererComponent(isSelected: Boolean,
        hasFocus: Boolean, row: Int, column: Int): Component = {
      if (hasFocus) tryMove(row, column)
      renderCell(row, column)
    }
  }

  private val message = new Label("X's move")

  private val mainFrame = new MainFrame {
    title = "Tic Tac Toe"
    location = new Point(200, 200)
    resizable = false
    message.horizontalTextPosition = Alignment.Center
    contents = new BoxPanel(Orientation.Vertical) {
      contents += message
      contents += table
    }
  }

  private def renderCell(row: Int, column: Int): Component = GameBoard.getPlayer(row, column) match {
    case 1 => x
    case 2 => o
    case _ => empty
  }

  def start() {
    SwingUtilities.invokeLater(new Runnable {
      def run {
        mainFrame.visible = true
      }
    })
  }

  def tryMove(row: Int, column: Int) {
    selection = (row -> column)
  }

  def setWinner(player: Option[Player]) {
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

  def update(player: Player) {
    table.repaint
    player match {
      case _: Player1 =>
        message.text = "X's move"
      case _: Player2 =>
        message.text = "O's move"
    }
  }

  private var selection = (-1 -> -1)

  def awaitMoveSelection: (Int, Int) = {
    refreshSelection()
    while (!hasChosenMove || !legalSelection) {
      Thread.sleep(500)
    }

    selection
  }

  private def hasChosenMove: Boolean = {
    val (row, column) = selection
    if (row > -1 && column > -1) true
    else false
  }

  private def legalSelection: Boolean = {
    val (row, column) = selection
    if (GameBoard.getPlayer(row, column) == 0) true
    else false
  }

  private def refreshSelection() {
    selection = (-1 -> -1)
  }
}

import scala.swing._

import java.awt.{Dimension, Color}
import javax.swing.{ImageIcon, SwingUtilities}

object GUI {

  private val whiteDisk = new Label { icon = new ImageIcon("images/white.gif") }
  private val blackDisk = new Label { icon = new ImageIcon("images/black.gif") }
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

  private val message = new Label("Your move")

  private val mainFrame = new MainFrame {
    title = "Tic Tac Toe"
    location = new Point(200, 200)
    resizable = false
    contents = new BoxPanel(Orientation.Vertical) {
      contents += message
      contents += table
    }
  }

  private def renderCell(row: Int, column: Int): Component = GameBoard.getPlayer(row, column) match {
    case 1 => blackDisk
    case 2 => whiteDisk
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
      case Some(_: Human) =>
        message.text = "You won!"
      case Some(_: Computer) =>
        message.text = "Better luck next time..."
      case _ =>
        message.text = "It's a tie!"
    }
  }

  def update(player: Player) {
    table.repaint
    player match {
      case _: Human =>
        message.text = "Your move"
      case _: Computer =>
        message.text = "CPU is thinking"
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

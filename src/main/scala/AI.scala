trait MaxMin
case object Max extends MaxMin
case object Min extends MaxMin

object AI extends Utilities {
  type FitnessMove = Tuple2[Int, Option[Move]]

  def terminal(node: Board)(implicit players: Players) =
    if (!node.getWinner.isEmpty) true
    else
      node.checkIfBoardIsFull match {
        case Running => false
        case _ => true
      }

  def not(p: Player)(implicit players: Players) = p match {
    case _: Player1 => players._2
    case _: Player2 => players._1
  }

  def max(x: FitnessMove, y: FitnessMove) = if (x._1 >= y._1) x else y
  def min(x: FitnessMove, y: FitnessMove) = if (x._1 <= y._1) x else (y._1, x._2)

  def search(board: Board, player: Player)(implicit players: Players): Move = {
    def alphaBeta(node: Board, alpha: Int, beta: Int, moveChoice: Option[Move], player: Player,
        p: MaxMin): FitnessMove =
      if (terminal(node))
        (node.fitness, moveChoice)
      else
        p match {
          case Max =>
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foldLeft((alpha, moveChoice)) { case ((alpha, moveChoice), move) =>
              val simulatedBoard = node.simulate(move, player)
              max((alpha, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, Option(move), not(player), Min))
            }

          case Min =>
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foldLeft((beta, moveChoice)) { case ((beta, _), move) =>
              val simulatedBoard = node.simulate(move, player)
              min((beta, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, moveChoice, not(player), Max))
            }
        }
    val (_, moveChoice) = alphaBeta(board, Integer.MIN_VALUE, Integer.MAX_VALUE, None, player, Max)
    moveChoice.get
  }

}

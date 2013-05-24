trait MaxMin
object Max extends MaxMin
object Min extends MaxMin

object AI extends Utilities {
  type FitnessMove = Tuple2[Int, Option[Move]]

  def terminal(node: Board, players: Players) =
    if (!node.getWinner(players).isEmpty) true
    else
      node.checkIfBoardIsFull match {
        case _: Running => false
        case _ => true
      }

  def not(p: Player, players: Players) = p match {
    case _: Player1 => players._2
    case _: Player2 => players._1
  }

  def max(x: FitnessMove, y: FitnessMove) = if (x._1 >= y._1) x else y
  def min(x: FitnessMove, y: FitnessMove) = if (x._1 <= y._1) x else (y._1, x._2)

  def search(board: Board, player: Player, players: Players): Move = {
    def alphaBeta(node: Board, alpha: Int, beta: Int, moveChoice: Option[Move], player: Player,
        p: MaxMin): FitnessMove =
      if (terminal(node, players))
        (node.fitness(players), moveChoice)
      else
        p match {
          case _: Max.type =>
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foldLeft((alpha, moveChoice)) { case ((alpha, moveChoice), move) =>
              val simulatedBoard = node.simulate(move, player)
              max((alpha, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, Option(move), not(player, players), Min))
            }

          case _: Min.type =>
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foldLeft((beta, moveChoice)) { case ((beta, _), move) =>
              val simulatedBoard = node.simulate(move, player)
              min((beta, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, moveChoice, not(player, players), Max))
            }
        }
    val (_, moveChoice) = alphaBeta(board, Integer.MIN_VALUE, Integer.MAX_VALUE, None, player, Max)
    moveChoice.get
  }

}

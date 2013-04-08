trait MaxMin
object Max extends MaxMin
object Min extends MaxMin

object AI extends Utilities {
  type FitnessMove = Tuple2[Int, Option[Move]]

  def terminal(node: Board, players: Players) =
    if (!node.checkForWinner(players).isEmpty) true
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
  def min(x: FitnessMove, y: FitnessMove) = if (x._1 <= y._1) x else y

  def search(board: Board, player: Player, players: Players): Move = {
    def alphaBeta(node: Board, a: Int, b: Int, r: Option[Move], player: Player,
      p: MaxMin): FitnessMove = {
      var alpha = a
      var beta = b
      var moveChoice = r
      if (terminal(node, players))
        (node.fitness(players), moveChoice)
      else
        p match {
          case _: Max.type => {
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foreach { move =>
              val simulatedBoard = node.simulate(move, player)
              val max1 =
                max((alpha, moveChoice),
                  alphaBeta(simulatedBoard, alpha, beta, Option(move), not(player, players), Min))
              alpha = max1._1
              moveChoice = max1._2
            }
            (alpha, moveChoice)
          }

          case _: Min.type => {
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foreach { move =>
              val simulatedBoard = node.simulate(move, player)
              val min1 = 
                min((beta, moveChoice),
                  alphaBeta(simulatedBoard, alpha, beta, moveChoice, not(player, players), Max))
              beta = min1._1
            }
            (beta, moveChoice)
          }
        }
    }
    val (_, moveChoice) = alphaBeta(board, Integer.MIN_VALUE, Integer.MAX_VALUE, None, player, Max)
    moveChoice.get
  }

}

trait MaxMin
object Max extends MaxMin
object Min extends MaxMin

object AI extends Utilities {
  type FitnessMove = Tuple2[Int, Option[Move]]
  val player1 = new Human
  val player2 = new Computer

  def terminal(node: Board) =
    if (!node.checkForWinner.isEmpty) true
    else
      node.checkIfBoardIsFull match {
        case _: Running => false
        case _ => true
      }

  def not(p: Player) = p match {
    case _: Human => player2
    case _: Computer => player1
  }

  def max(x: FitnessMove, y: FitnessMove) = if (x._1 >= y._1) x else y
  def min(x: FitnessMove, y: FitnessMove) = if (x._1 <= y._1) x else y

  def search(board: Board, player: Player): Move = {
    def alphaBeta(node: Board, a: Int, b: Int, r: Option[Move], player: Player,
      p: MaxMin): FitnessMove = {
      var alpha = a
      var beta = b
      var moveChoice = r
      if (terminal(node))
        (node.fitness, moveChoice)
      else
        p match {
          case _: Max.type => {
            node.findPossibleMoves.
            //takeWhile(_ => beta > alpha).
            foreach { move =>
              val simulatedBoard = node.simulate(move, player)
              val max1 =
                max((alpha, moveChoice),
                  alphaBeta(simulatedBoard, alpha, beta, Option(move), not(player), Min))
              alpha = max1._1
              moveChoice = max1._2
            }
            (alpha, moveChoice)
          }

          case _: Min.type => {
            node.findPossibleMoves.
            //takeWhile(_ => beta > alpha).
            foreach { move =>
              val simulatedBoard = node.simulate(move, player)
              val min1 = 
                min((beta, moveChoice),
                  alphaBeta(simulatedBoard, alpha, beta, moveChoice, not(player), Max))
              beta = min1._1
            }
            (beta, moveChoice)
          }
        }
    }

    def alphaBeta2(node: Board, alpha: Int, beta: Int, moveChoice: Option[Move], player: Player,
      p: MaxMin): FitnessMove = {
      if (terminal(node))
        (node.fitness, moveChoice)
      else
        p match {
          case _: Max.type =>
            node.findPossibleMoves.
            //takeWhile(_ => beta > alpha).
            foldLeft((alpha, moveChoice)) { case ((alpha, moveChoice), move) =>
              val simulatedBoard = node.simulate(move, player)
              max((alpha, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, Option(move), not(player), Min))
            }
          case _: Min.type =>
            node.findPossibleMoves.
            //takeWhile(_ => beta > alpha).
            foldLeft((beta, moveChoice)) { case ((beta, _), move) =>
              val simulatedBoard = node.simulate(move, player)
              min((beta, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, moveChoice, not(player), Max))
            }
        }
    }
    val (_, moveChoice) = alphaBeta(board, Integer.MIN_VALUE, Integer.MAX_VALUE, None, player, Max)
    moveChoice.get
  }

}

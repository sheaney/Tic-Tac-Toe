trait MaxMin
case class Max() extends MaxMin
case class Min() extends MaxMin

object AI extends Utilities {
  type FitnessMove = Tuple2[Int, Option[Move]]
  //type FitnessMove = Tuple2[Int, List[Move]]
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
        (node.fitness2, moveChoice)
      else
        p match {
          case _: Max => {
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foreach { move =>
              val simulatedBoard = node.simulate(move, player)
              val max1 =
                max((alpha, moveChoice),
                  alphaBeta(simulatedBoard, alpha, beta, Option(move), not(player), Min()))
              alpha = max1._1
              moveChoice = max1._2
            }
            (alpha, moveChoice)
          }

          case _: Min => {
            node.findPossibleMoves.
            takeWhile(_ => beta > alpha).
            foreach { move =>
              val simulatedBoard = node.simulate(move, player)
              val min1 =
                min((beta, moveChoice),
                  alphaBeta(simulatedBoard, alpha, beta, moveChoice, not(player), Max()))
              beta = min1._1
            }
            (beta, moveChoice)
          }
        }

    }

    val (f, moveChoice) = alphaBeta(board, Integer.MIN_VALUE, Integer.MAX_VALUE, None, player, Max())
    println(moveChoice.head)
    moveChoice.get
  }

}

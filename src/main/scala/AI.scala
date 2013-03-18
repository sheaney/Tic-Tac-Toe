trait MaxMin
case class Max() extends MaxMin
case class Min() extends MaxMin

object AI extends Utilities {
  type FitnessMove = Tuple2[Int, List[Move]]
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

    def alphaBeta(node: Board, alpha: Int, beta: Int, moveChoice: List[Move], player: Player,
      p: MaxMin): FitnessMove = {
      if (terminal(node)) {
        //println(f"Fitness: ${node.fitness} \n $node")
        (node.fitness, moveChoice)
      }
      else
        p match {
          case _: Max =>
            node.findPossibleMoves.
            //takeWhile(_ => beta > alpha).
            foldLeft((alpha, moveChoice)) { case ((alpha, moveChoice), move) =>
              val simulatedBoard = node.simulate(move, player)
              max((alpha, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, move :: moveChoice, not(player), Min()))
            }

          case _: Min =>
            node.findPossibleMoves.
            //takeWhile(_ => beta > alpha).
            foldLeft((alpha, moveChoice)) { case ((alpha, moveChoice), move) =>
              val simulatedBoard = node.simulate(move, player)
              (
              min((beta, moveChoice),
                alphaBeta(simulatedBoard, alpha, beta, moveChoice, not(player), Min()))._1,
              moveChoice
              )
            }
        }
    }
    val (f, moveChoice) = alphaBeta(board, Integer.MIN_VALUE, Integer.MAX_VALUE, List.empty[Move], player, Max())
    println(s"Chosen fitness: $f")
    moveChoice.head
  }

}

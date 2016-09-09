class Puzzle(val width: Int,
             val height: Int,
             val board: Board
            ) {


  def solve(): Board = {
    val rulesBoard = this.board.copy()
    Rules.applyRules(rulesBoard)

    val solved = rulesBoard.getSolvedSegments()
    bruteforce(rulesBoard, solved.head, solved.tail).get
  }

  private def bruteforce(board: Board, link: List[(Int, Int)], rest: List[List[(Int, Int)]]): Option[Board] = {
    if (link.head == link.last && rest.isEmpty) {
      // TODO: Optimize
      for(y <- 0 until height) {
        for(x <- 0 until width) {
          val num = board.getNumber(x, y)
          if (num.isDefined && board.getSideCount(x, y) != num.get) {
            //println(s"Expected $num, got ${board.getSideCount(x, y)} at ($x, $y)")
            //println(board)
            return None
          }
        }
      }
      return Some(board)
    }

    val lastMove = link.takeRight(2)
    val (cx, cy) = lastMove.last

    // TODO: Optimize
    // Find current square, and check if too many sides are set:
    val (sx, sy) = (scala.math.min(link.head._1, link.last._1), scala.math.min(link.head._2, link.last._2))
    val num = board.getNumber(sx, sy)
    val numSides = board.getSideCount(sx, sy)
    if (num.isDefined && numSides > num.get) return None
    if (board.isFinishedProcessing(sx, sy) && num.isDefined && numSides != num.get) return None

    // Find direction of last move, then where we can move from here:
    val direction = (lastMove.last._1 - lastMove.head._1, lastMove.last._2 - lastMove.head._2)
    val allPossibleMoves = direction match {
      case (0, -1) => List((0, 0 - 1), (0 + 1, 0), (0 - 1, 0))
      case (1, 0) => List((0 + 1, 0), (0, 0 + 1), (0, 0 - 1))
      case (0, 1) => List((0, 0 + 1), (0 + 1, 0), (0 - 1, 0))
      case (-1, 0) => List((0 - 1, 0), (0, 0 + 1), (0, 0 - 1))
      case _ => List()
    }

    //val boundedMoves = allPossibleMoves.filter(m => cx + m._1 >= 0 && cy + m._2 >= 0 && cx + m._1 <= width && cy + m._2 <= height)
//    val checkedMoves = allPossibleMoves.filter(m => {
//      val pos = (cx + m._1, cy + m._2)
//      (pos._1 > 0 && pos._2 > 0 && pos._1 <= width && pos._2 <= height) && (pos == link.head || !link.contains(pos))
//    })

    // Limit options:
    val boundedMoves = allPossibleMoves.filter(m => cx + m._1 >= 0 && cy + m._2 >= 0 && cx + m._1 <= width && cy + m._2 <= height)
    val checkedMoves = boundedMoves.filter(m => (cx + m._1, cy + m._2) == link.head || !link.contains((cx + m._1, cy + m._2)))

    // If any of the possible moves are definitely set, then go there:
    val finalMoves = checkedMoves.filter(m => {
      m match {
        case (0, -1) => board.getLineUp(cx, cy).contains(true)
        case (1, 0) => board.getLineRight(cx, cy).contains(true)
        case (0, 1) => board.getLineDown(cx, cy).contains(true)
        case (-1, 0) => board.getLineLeft(cx, cy).contains(true)
      }
    })

    // If above list is empty, fall back to the other moves:
    val moves = if (finalMoves.nonEmpty) finalMoves else checkedMoves
    moves.toStream.map(move => {
      val newBoard = board.copy()
      move match {
        case (0, -1) => newBoard.setLineUp(cx, cy, true)
        case (1, 0) => newBoard.setLineRight(cx, cy, true)
        case (0, 1) => newBoard.setLineDown(cx, cy, true)
        case (-1, 0) => newBoard.setLineLeft(cx, cy, true)
      }
      val newLast = (cx + move._1, cy + move._2)
      val newRest = rest.filter(r => r != List(link.last, newLast) && r != List(newLast, link.last))

      if (link.contains(newLast) && newLast != link.head) return None
      bruteforce(newBoard, link ++ List(newLast), newRest)

    }).collectFirst { case p if p.isDefined => p.get}
  }
}

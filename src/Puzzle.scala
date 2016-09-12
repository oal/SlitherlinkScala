//import scala.concurrent.duration._
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.{Await, Future}

class Puzzle(val width: Int,
             val height: Int,
             val board: Board
            ) {

  // "constants" for the four directions.
  val UP = (0, -1)
  val RIGHT = (1, 0)
  val DOWN = (0, 1)
  val LEFT = (-1, 0)

  // Helper used to get the "square" we just moved into, so we can check that it doesn't have too many sides set.
  private def segmentToSquareCoord(seg: List[(Int, Int)]) = {
    val (sx, sy) = (
      scala.math.min(seg.head._1, seg.last._1),
      scala.math.min(seg.head._2, seg.last._2)
      )

    if (sx == width) {
      if (sy == height) {
        (sx - 1, sy - 1)
      } else {
        (sx - 1, sy)
      }
    } else {
      if (sy == height) {
        (sx, sy - 1)
      } else {
        (sx, sy)
      }
    }
  }

  def solve(): Board = {
    // Special handling for 1x1
    if(width == 1 && height == 1) {
      this.board.setTop(0, 0, true)
      this.board.setRight(0, 0, true)
      this.board.setBottom(0, 0, true)
      this.board.setLeft(0, 0, true)
      return this.board
    }

    val rulesBoard = this.board.copy()
    Rules.applyRules(rulesBoard)
    val solved = rulesBoard.getSolvedSegments().distinct
    bruteforce(rulesBoard, solved.head, solved.tail).get

    /*val tasks: Seq[Future[Board]] = for (i <- solved.indices) yield Future {
      val start = solved(i)
      println(s"Starting $i at $start")
      val board = bruteforce(rulesBoard, start, solved.filter(_!=start)).get
      println(s"Finished $i at $start")
      board
    }
    Await.result(Future.firstCompletedOf(tasks), 120.seconds)*/
  }

  private def bruteforce(board: Board, link: List[(Int, Int)], rest: List[List[(Int, Int)]]): Option[Board] = {
    // Last move is the segment consisting of the two last elements of the link
    val lastMove = link.takeRight(2)
    val (cx, cy) = lastMove.last // Current x and current y positions.

    // Find current square, and check if too many sides are set:
    val (sx, sy) = segmentToSquareCoord(lastMove)
    val num = board.getNumber(sx, sy)
    val numSides = board.getSideCount(sx, sy)
    if (num.isDefined && numSides > num.get) return None

    // Check if we're back to our starting position, and none of the pre-solved line segments are left in the "rest" list.
    if (link.head == link.last && rest.isEmpty) {
      // TODO: Optimize?
      // Verify that the whole board is valid.
      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val num = board.getNumber(x, y)
          if (num.isDefined && board.getSideCount(x, y) != num.get) {
            return None
          }
        }
      }
      // Return up the chain!
      return Some(board)
    }

    // Find direction of last move, then where we can move from here:
    val direction = (
      lastMove.last._1 - lastMove.head._1,
      lastMove.last._2 - lastMove.head._2)

    val allPossibleMoves = direction match {
      case UP => List(UP, RIGHT, LEFT)
      case RIGHT => List(UP, RIGHT, DOWN)
      case DOWN => List(RIGHT, LEFT, DOWN)
      case LEFT => List(UP, LEFT, DOWN)
      case _ => List()
    }

    // TODO: Turn into one filter call
    // Limit options:
    val boundedMoves = allPossibleMoves.filter(m => cx + m._1 >= 0 && cy + m._2 >= 0 && cx + m._1 <= width && cy + m._2 <= height)
    val checkedMoves = boundedMoves.filter(m => (cx + m._1, cy + m._2) == link.head || !link.contains((cx + m._1, cy + m._2)))

    // If any of the possible moves are definitely set, then go there:
    val finalMoves = checkedMoves.filter(m => {
      m match {
        case UP => board.getLineUp(cx, cy).contains(true)
        case RIGHT => board.getLineRight(cx, cy).contains(true)
        case DOWN => board.getLineDown(cx, cy).contains(true)
        case LEFT => board.getLineLeft(cx, cy).contains(true)
      }
    })

    // If above list is empty, fall back to the other moves:
    val moves = if (finalMoves.nonEmpty) finalMoves else checkedMoves

    // Turn moves into a stream so it can be lazily evaluated, and we can collect the first valid board off of it.
    moves.toStream.map(move => {
      // Create copy of board, and update it with the line to the next position of our slitherlink
      val newBoard = board.copy()
      move match {
        case UP => newBoard.setLineUp(cx, cy, true)
        case RIGHT => newBoard.setLineRight(cx, cy, true)
        case DOWN => newBoard.setLineDown(cx, cy, true)
        case LEFT => newBoard.setLineLeft(cx, cy, true)
      }

      // Find what the new last element of the link is (cx and cy for the next iteration)
      val newLast = (cx + move._1, cy + move._2)

      // If the segment we just inserted was inside the rest list, remove it from there
      val newRest = rest.filter(r => r != List(link.last, newLast) && r != List(newLast, link.last))

      // Next iteration of our brute force attack.
      bruteforce(newBoard, link ++ List(newLast), newRest)
    }).collectFirst { case p if p.isDefined => p.get } // Get the first valid board.
  }
}

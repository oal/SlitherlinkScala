import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

class BoolRef(var set: Boolean)

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

    // Apply rules
    val rulesBoard = this.board.copy()
    Rules.applyRules(rulesBoard)

    // Sort solved segments so that the ones close to the center are where we start.
    // Benchmarks show that this is a good idea as we can't easily get stuck in a corner.
    val solved = rulesBoard.getSolvedSegments().distinct.sortBy(segment => {
      val p = segment.head
      val x = p._1-board.width/2
      val y = p._2-board.height/2
      math.sqrt(x*x+y*y)
    })

    // Find start positions. Take every second solved segment, otherwise we'll often end up with
    // several very similar starting positions. Only grab the first four, as that will most likely
    // schedule all to run at the same time.
    val startPositions = if(solved.size < 8) solved.indices
    else solved.indices.grouped(2).map(_.head).toList.slice(0, 4)

    // Create tasks to be run concurrently. Use the BoolRef as a pass-by-reference boolean that
    // is set to true once one valid board is found, effectively terminating the rest of the tasks.
    val done = new BoolRef(false)
    val tasks: Seq[Future[Option[Board]]] = for (i <- startPositions) yield Future {
      val start = solved(i)
      val board = bruteforce(rulesBoard, start, solved.filter(_!=start), done)
      if(board.isDefined) synchronized { done.set = true }
      board
    }

    val aggregated: Future[Seq[Option[Board]]] = Future.sequence(tasks)
    Await.result(aggregated, Duration.Inf).filter(_.isDefined).head.get
  }

  private def bruteforce(board: Board, link: List[(Int, Int)], rest: List[List[(Int, Int)]], done: BoolRef): Option[Board] = {
    if(done.set) return None

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
      case RIGHT => List(RIGHT, UP, DOWN)
      case DOWN => List(DOWN, RIGHT, LEFT)
      case LEFT => List(LEFT, UP, DOWN)
      case _ => List()
    }

    // TODO: Turn into one filter call
    // Limit options:
    val boundedMoves = allPossibleMoves.filter(m => cx + m._1 >= 0 && cy + m._2 >= 0 && cx + m._1 <= width && cy + m._2 <= height)
    val checkedMoves = boundedMoves.filter(m => (cx + m._1, cy + m._2) == link.head || !link.contains((cx + m._1, cy + m._2)))
    val exDeniedMoves = checkedMoves.filter(m => {
      m match {
        case UP => if(board.getLineUp(cx, cy).contains(false)) false else true
        case RIGHT => if(board.getLineRight(cx, cy).contains(false)) false else true
        case DOWN => if(board.getLineDown(cx, cy).contains(false)) false else true
        case LEFT => if(board.getLineLeft(cx, cy).contains(false)) false else true
      }
    }).sortBy(m => {
      val heads = rest.map(s => s.head)
      val tails = rest.map(s => s.last)
      val to = (cx+m._1, cy+m._2)
      if(heads.contains(to) || tails.contains(to)) -1 else 1
    })

    // If any of the possible moves are definitely set, then go there:
    val finalMoves = exDeniedMoves.filter(m => {
      m match {
        case UP => board.getLineUp(cx, cy).contains(true)
        case RIGHT => board.getLineRight(cx, cy).contains(true)
        case DOWN => board.getLineDown(cx, cy).contains(true)
        case LEFT => board.getLineLeft(cx, cy).contains(true)
      }
    })

    // If above list is empty, fall back to the other moves:
    val moves = if (finalMoves.nonEmpty) finalMoves else exDeniedMoves

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
      bruteforce(newBoard, link ++ List(newLast), newRest, done)
    }).collectFirst { case p if p.isDefined => p.get } // Get the first valid board.
  }
}

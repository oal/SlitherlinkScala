import scala.annotation.tailrec
import scala.util.Random

class Puzzle(val width: Int,
             val height: Int,
             val board: Board) {

  val TOP = 0
  val RIGHT = 1
  val BOTTOM = 2
  val LEFT = 3

  def solve() = {
    applyRules()

    println()

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        //println(board.getSquare(x, y).solutions.length)
        println(x, y, board.getSquare(x, y).solutions)
      }
    }

    println(this.board)

    val linkStart = board.getFirstSolved.get.toLine.get
//
//    val b2 = this.board.copy()
//
//    b2.setTop(0, 0, true)
//
//    println( "---")
//    println(b2)
//    println(this.board)
//    println( "---")
//
//    println(b2.getSquare(0, 0).solutions)
    bruteforce(this.board.copy(), linkStart)
  }
    def bruteforce(board: Board, lastSegment: List[(Int, Int)] = List()): Option[Board] = {
      val (x, y) = lastSegment.last

      val up = board.getLineUp(x, y)
      val right = board.getLineRight(x, y)
      val down = board.getLineDown(x, y)
      val left = board.getLineLeft(x, y)

      val direction = (lastSegment.last._1 - lastSegment.head._1, lastSegment.last._2 - lastSegment.head._2)

      board.setLineDown(x, y, true)

      println(s"Dir: $direction ($lastSegment)")
      println(board)

      None
    }
//  def bruteforce(link: List[(Int, Int)] = List()): Option[List[(Int, Int)]] = {
//    val (x, y) = link.last
//
//    val up = board.getUp(x, y)
//    val right = board.getRight(x, y)
//    val down = board.getDown(x, y)
//    val left = board.getLeft(x, y)
//
//    val lastMove = link.takeRight(2)
//    val direction = (lastMove.last._1 - lastMove.head._1, lastMove.last._2 - lastMove.head._2)
//    println(direction, "asd", link)
//    println(
//      x, y,
//      board.getUp(x, y),
//      board.getRight(x, y),
//      board.getDown(x, y),
//      board.getLeft(x, y)
//    )
//
//
//    if (up.contains(true) && direction != (0, 1)) {
//      return bruteforce(link ++ List((x, y - 1)))
//    }
//    if (right.contains(true) && direction != (-1, 0)) {
//      return bruteforce(link ++ List((x + 1, y)))
//    }
//    if (down.contains(true) && direction != (0, -1)) {
//      return bruteforce(link ++ List((x, y + 1)))
//    }
//    if (left.contains(true) && direction != (1, 0)) {
//      return bruteforce(link ++ List((x - 1, y)))
//    }
//
//    val s = Stream[Boolean](up.getOrElse(true), right.getOrElse(true), down.getOrElse(true), left.getOrElse(true))
//
//    s.map(nextMove => bruteforce(link ++ List(nextMove))).collectFirst { case p if p.isDefined => p.get }
//  }

  def applyRules() = {
    ruleTwoInCorner()
  }

  def ruleTwoInCorner() = {
    val topRight = board.getSquare(width - 1, 0)
    if (topRight.number.contains(2)) {
      board.setTop(width - 2, 0, true)
      board.setRight(width - 1, 1, true)
    }
  }
}

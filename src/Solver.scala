import Solver.Direction.Direction

import scala.annotation.tailrec

object Solver {

  // Because apparently this is how scala does enums.
  object Direction extends Enumeration {
    type Direction = Value
    val Up, Down, Left, Right = Value
  }

  class Puzzle(val width: Int,
               val height: Int,
               val numbers: List[List[Option[Int]]],
               val linesHorizontal: List[List[Boolean]],
               val linesVertical: List[List[Boolean]]
              ) {

    def solve(): Puzzle = {
      val horizontal = linesHorizontal.take(3) ++ List(List.fill(width)(true)) ++ linesHorizontal.drop(3)
      val vertical = linesVertical.take(3) ++ List(List.fill(height)(true)) ++ linesVertical.drop(3)

      new Puzzle(width, height, numbers, horizontal, vertical)
    }

    override def toString: String = {
      val horizontal = linesHorizontal.map(line => {
        "+" + line.map(segment => if(segment) "-" else " ").mkString("+") + "+"
      })
      val vertical = (0 until height).map(i => {
        linesVertical.map(line => line(i)).map(segment => if(segment) "|" else " ").mkString(" ")
      })


      (0 until height).map(i => {
        horizontal(i) +"\n" + vertical(i)
      }).mkString("\n") + "\n" + List.fill(width+1)("+").mkString(" ") + "\n"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    val nums = lines.map(line => line.split(" ").toList.map(s => Utils.toInt(s)))
    val linesHorizontal = (0 until width).toList.map(x => List.fill(width)(false))
    val linesVertical = (0 until height).toList.map(y => List.fill(width)(false))

    new Puzzle(width, height, nums, linesHorizontal, linesVertical)
  }

  def parseBoards(lines: List[String]): List[Puzzle] = {
    // Pattern match width / height
    val Array(width, height) = lines.head.split("x").map(s => s.toInt)
    parseBoard(width, height, lines.tail.take(height))
    val thisBoard = List(parseBoard(width, height, lines.tail.take(height)))

    // Recursively parse the rest of the boards
    val nextBoardData = lines.drop(height + 1) // +1 for the size line
    if (nextBoardData.nonEmpty) thisBoard ++ parseBoards(nextBoardData)
    else thisBoard
  }
}
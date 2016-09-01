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
               val linesVertical: List[List[Boolean]],
               val solveIdx: Int = 0,
               val config: Int = 0
              ) {

    def setHorizontal(x: Int, y: Int): List[List[Boolean]] = {
      if (x < 0 || y < 0 || x >= width || y >= height) return linesHorizontal
      val mutLine = linesHorizontal(y)

      linesHorizontal.take(y) ++ List(
        mutLine.take(x) ++ List(true) ++ mutLine.drop(x + 1)
      ) ++ linesHorizontal.drop(y + 1)
    }

    def setVertical(x: Int, y: Int): List[List[Boolean]] = {
      if (x < 0 || y < 0 || x >= width || y >= height) return linesVertical
      val mutLine = linesVertical(x)

      linesVertical.take(x) ++ List(
        mutLine.take(y) ++ List(true) ++ mutLine.drop(y + 1)
      ) ++ linesVertical.drop(x + 1)
    }

    @tailrec final def solve(): Puzzle = {
      println("------------------------")
      println(this)
      println(solveIdx, config, (solveIdx % (width), solveIdx / height))
      if (solveIdx >= width * height) this
      else {
        val useConfig = config match {
          case 0 => List() // Leave empty
          case 1 => List((-1, 0), (0, -1)) // Left, up
          case 2 => List((1, 0), (0, -1)) // Right, up
          case 3 => List((-1, 0), (0, 1)) // Left, down
          case 4 => List((1, 0), (0, 1)) // Right, down
        }

        val nextConfig = (config+1)%5
        val nextSolveIdx = if(config > 0 && nextConfig == 0) solveIdx + 1 else solveIdx

        if (useConfig.nonEmpty) {
          val x = solveIdx % (width)
          val y = solveIdx / height
          val newVert = setVertical(x, y + useConfig(1)._2)
          val newHoriz = setHorizontal(x + useConfig.head._1, y)
          new Puzzle(width, height, numbers, newHoriz, newVert, nextSolveIdx, nextConfig).solve()
        } else new Puzzle(width, height, numbers, linesHorizontal, linesVertical, nextSolveIdx, nextConfig).solve()
      }
    }

    override def toString: String = {
      val horizontal = linesHorizontal.map(line => {
        "+" + line.map(segment => if (segment) "-" else " ").mkString("+") + "+"
      })
      val vertical = (0 until height).map(i => {
        linesVertical.map(line => line(i)).map(segment => if (segment) "|" else " ").mkString(" ")
      })

      s"${width}x${height}\n" + (0 until height).map(i => {
        horizontal(i) + "\n" + vertical(i)
      }).mkString("\n") + "\n" + List.fill(width + 1)("+").mkString(" ") + "\n"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    val nums = lines.map(line => line.split(" ").toList.map(s => Utils.toInt(s)))
    val linesHorizontal = (0 until height).toList.map(x => List.fill(width)(false))
    val linesVertical = (0 until width + 1).toList.map(y => List.fill(height)(false))

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
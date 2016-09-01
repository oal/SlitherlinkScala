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
               val rows: List[List[Option[Int]]],
               val start: (Int, Int),
               val link: List[Direction]) {

    import Direction._

    // Get number inside cell
    def getNumber(x: Int, y: Int): Option[Int] = {
      if (x < 0 || y < 0 || x > width || y > height) None
      else rows(y)(x)
    }

    def currPosition(): (Int, Int) = {
      val (x, y) = link.map {
        case Up => (0, -1)
        case Down => (0, 1)
        case Left => (-1, 0)
        case Right => (1, 0)
        case _ => (0, 0)
      }.reduce((a, b) => (a._1 + b._1, a._2 + b._2))

      (x + start._1, y + start._2)
    }

    def linkCoords: List[(Int, Int)] = {
      val steps = link.map {
        case Up => (0, -1)
        case Down => (0, 1)
        case Left => (-1, 0)
        case Right => (1, 0)
        case _ => (0, 0)
      }

      val withStart: List[(Int, Int)] = start :: steps
      withStart.drop(1).scanLeft(withStart.head) {
        case (r, c) => (r._1 + c._1, r._2 + c._2)
      }
    }

    def lineSegments(): List[List[(Int, Int)]] = {
      linkCoords.sliding(2).toList
    }

    def countLines(x: Int, y: Int): Int = {
      val els = List(
        List((x, y), (x+1, y)),
        List((x+1, y), (x, y)),

        List((x, y), (x, y+1)),
        List((x, y+1), (x, y)),

        List((x+1, y), (x+1, y+1)),
        List((x+1, y+1), (x+1, y)),

        List((x, y+1), (x+1, y+1)),
        List((x+1, y+1), (x, y+1))
      ).toSet

      lineSegments().toSet.intersect(els).size
    }

    def lastMoveLegal(): Boolean = {
      val segments = lineSegments()

      val last = segments.last
      val lastReverse = List(last(1), last.head)

      val rest = segments.drop(1).dropRight(1).toSet
      val checkSegments = Set(
        last,
        lastReverse,
        List(last(1), (last(1)._1+1, last(1)._2)),
        List(last(1), (last(1)._1, last(1)._2+1)),
        List(last(1), (last(1)._1-1, last(1)._2)),
        List(last(1), (last(1)._1, last(1)._2-1))
      )

      rest.intersect(checkSegments).isEmpty
    }

    def validateNumbers(): Boolean = {
      val matches = (0 until height).map(y => (0 until width).map(x => {
        val num = getNumber(x, y)
        if (num.isDefined) {
          val neededNum = num.get
          //println(x, y, neededNum, countLines(x, y))
          countLines(x, y) == neededNum
        }
        else true
      }).find(el => !el)).find(el => el.isDefined && !el.get)

      matches.isEmpty
    }

    // Recursive solver
    private def solveStep(): Option[Puzzle] = {
      val curr = currPosition()
      val (x, y) = curr
      if (x < 0 || y < 0 || x > width || y > height) return None

      if(!lastMoveLegal()) return None

      if (curr == start) {
        //val r = scala.util.Random
        //val i = r.nextInt(100000)
        //if(i >= 99990) println(this)
        //println(validateNumbers())
        //println(lineSegments())
        //println(countLines(0, 0), "LINES")
        if(validateNumbers()) return Option(this)
        else return None
      }

      /*val next = link.last match {
        case Up => Stream(Up, Left, Right)
        case Down => Stream(Down, Left, Right)
        case Left => Stream(Left, Up, Down)
        case Right => Stream(Right, Up, Down)
      }*/

      new Puzzle(width, height, rows, start, link ++ List(Up)).solveStep().orElse(
        new Puzzle(width, height, rows, start, link ++ List(Right)).solveStep().orElse(
          new Puzzle(width, height, rows, start, link ++ List(Down)).solveStep().orElse(
            new Puzzle(width, height, rows, start, link ++ List(Left)).solveStep()
          )
        )
      )

      //next.map(dir => new Puzzle(width, height, rows, start, link ++ List(dir)).solveStep()).find(_.isDefined).get
    }

    def solve(): Puzzle = {
      new Puzzle(width, height, rows, start, List(Up)).solveStep().orElse(
        new Puzzle(width, height, rows, start, List(Right)).solveStep().orElse(
          new Puzzle(width, height, rows, start, List(Down)).solveStep().orElse(
            new Puzzle(width, height, rows, start, List(Left)).solveStep()
          )
        )
      ).get
    }

    def toStringInput: String = {
      // Map over every row, drop rightmost item as that is only used in computation of solution.
      // Output number if any, otherwise a star is output. Join cells by space, and rows by newline.
      val text = rows.map(row => row.dropRight(1).map {
        case Some(i) => i
        case None => "*"
      }.mkString(" ")).mkString("\n")

      // Output should be identical to the input.
      s"$width x $height\n$text\n"
    }

    override def toString: String = {
      // Create pairs of two points: Each end of a line segment.
      val slidingCoords = lineSegments()

      // Filter out and organize horizontal and vertical lines.
      val horizontalLines: List[(Int, Int)] = slidingCoords.
        filter(pair => pair.head._2 == pair(1)._2).
        map(pair => (Math.min(pair.head._1, pair(1)._1), pair(1)._2))

      val verticalLines: List[(Int, Int)] = slidingCoords.
        filter(pair => pair.head._1 == pair(1)._1).
        map(pair => (pair(1)._1, Math.min(pair.head._2, pair(1)._2)))

      // Construct nested lists of strings based on where we have lines or not.
      val out = List.fill(height+1)("").zipWithIndex.map {
        case (s, iy: Int) =>
          List(
            List.fill(width+1)("").zipWithIndex.map {
              case (s2, ix: Int) => {
                if (horizontalLines.count { case (x, y) => x == ix && y == iy } > 0) "+-"
                else "+ "
              }
              case _ => "+ "
            },
            List.fill(width+1)("").zipWithIndex.map {
              case (s2, ix: Int) => {
                if (verticalLines.count { case (x, y) => x == ix && y == iy } > 0) "| "
                else "  "
              }
              case _ => "  "
            }
          )
      }

      // Combine all the small strings to one, with line breaks etc.
      val asString = out.map(doubleLine => doubleLine.map(
        line => line.mkString("")).mkString("\n")).mkString("\n")

      // Provide correct output. Trim to avoid line with only spaces at the end.
      s"$width x $height\n${asString.trim}"
    }
  }

  def parseBoard(width: Int, height: Int, lines: List[String]): Puzzle = {
    // We need a row extra because its' top line will be the above's bottom line.
    // That's because cells only have top and left lines, so we need partial cells
    // on the right and bottom.
    val bottomCells: List[Option[Int]] = List.fill(width + 1)(None)

    // Parse numbers to ints, or use None. Add one cell to the right (see above).
    val rows = lines.map(line => {
      line.split(" ").map(s => Utils.toInt(s)).toList ++ List(None)
    })

    // First, find a suitable start position (lowest possible number, but not 0).
    // Maybe there is a shorter way to achieve this?
    val minCell = rows.flatten.minBy(cell => {
      val num = cell.orElse(Option(0)).get
      if (num == 0) 5 // Set 0s to 5, as we never want to start at a 0
      else num // But we want to start at somewhere as low as possible
    })

    val idx = rows.flatten.indexOf(minCell)
    val x = idx % (width + 1)
    val y = idx / (height + 1)

    new Puzzle(width, height, rows ++ List(bottomCells), (x, y), List())
  }

  def parseBoards(lines: List[String]): List[Puzzle] = {
    // Pattern match width / height
    val Array(width, height) = lines.head.split("x").map(s => s.toInt)
    val thisBoard = List(parseBoard(width, height, lines.tail.take(height)))

    // Recursively parse the rest of the boards
    val nextBoardData = lines.drop(height + 1) // +1 for the size line
    if (nextBoardData.nonEmpty) thisBoard ++ parseBoards(nextBoardData)
    else thisBoard
  }
}
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

  def bruteforce(board: Board, link: List[(Int, Int)], rest: List[List[(Int, Int)]]): Option[Board] = {
    //println(board)
    if (link.head == link.last && rest.isEmpty) {
      // TODO: Optimize
      board.getNumberCoords.foreach { case (x, y, num) => {
        if (board.getSideCount(x, y) != num) {
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
    val (sx, sy) = (scala.math.min(link.head._1, link.last._1), scala.math.min(link.head._2, link.last._2))
    val num = board.getNumber(sx, sy)
    val numSides = board.getSideCount(sx, sy)
    if (num.isDefined && numSides > num.get) return None
    if (board.isFinishedProcessing(sx, sy)) {
      val num = board.getNumber(sx, sy)
      if (num.isDefined && numSides != num.get) return None
    }

    val direction = (lastMove.last._1 - lastMove.head._1, lastMove.last._2 - lastMove.head._2)

    val allPossibleMoves = direction match {
      case (0, -1) => List((0, 0 - 1), (0 + 1, 0), (0 - 1, 0))
      case (1, 0) => List((0 + 1, 0), (0, 0 + 1), (0, 0 - 1))
      case (0, 1) => List((0, 0 + 1), (0 + 1, 0), (0 - 1, 0))
      case (-1, 0) => List((0 - 1, 0), (0, 0 + 1), (0, 0 - 1))
      case _ => List()
    }

    val boundedMoves = allPossibleMoves.filter(m => cx + m._1 >= 0 && cy + m._2 >= 0 && cx + m._1 <= width && cy + m._2 <= height)
    val checkedMoves = boundedMoves.filter(m => (cx + m._1, cy + m._2) == link.head || !link.contains((cx + m._1, cy + m._2)))

    val allDirections = List((0, -1), (1, 0), (0, 1), (-1, 0))

    val finalMoves = checkedMoves.filter(m => {
      m match {
        case (0, -1) => board.getLineUp(cx, cy).contains(true)
        case (1, 0) => board.getLineRight(cx, cy).contains(true)
        case (0, 1) => board.getLineDown(cx, cy).contains(true)
        case (-1, 0) => board.getLineLeft(cx, cy).contains(true)
      }
    })

    val moves = if (finalMoves.nonEmpty) finalMoves /*++ checkedMoves.toSet.diff(finalMoves.toSet).toList*/ else checkedMoves

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

      //if (link.head == link.last && rest.isEmpty) return Some(newBoard)
      //println(newBoard)
      bruteforce(newBoard, link ++ List(newLast), newRest)

    }).collectFirst { case p if p.isDefined => {
      p.get
    }
    }

    //None
    /*val (mx, my) = boundedMoves.head
    (mx, my) match {
      case (0, -1) => board.setLineUp(cx, cy, true)
      case (1, 0) => board.setLineRight(cx, cy, true)
      case (0, 1) => board.setLineDown(cx, cy, true)
      case (-1, 0) => board.setLineLeft(cx, cy, true)
    }


    println(cx+mx, cy+my)
    println(board)*/
  }

  // Recursive solver
  /*private def solveStep(link: List[(Int, Int)], segmentsTail: List[List[(Int, Int)]]): Option[List[(Int, Int)]] = {
    //println(this.applyLinkCopy(link))
    //println(link.dropRight(1).count(_ == link.last))
    //val appliedBoard = this.applyLinkCopy(link)
    //println(appliedBoard)
    //println(segmentsTail)
    //if (link.count(_ == link.last) > 1 ) return None


    val (cx, cy) = link.last
    val lastMove = link.takeRight(2)

    val forcedMove = segmentsTail.filter(seg => (seg.head._1 == cx && seg.head._2 == cy) || (seg.last._1 == cx && seg.last._2 == cy))
    if (forcedMove.nonEmpty) {
      val move = forcedMove.head
      val segments = segmentsTail.filter(_ != move)
      if (link.last == move.head) solveStep(link ++ List(move.last), segments)
      else solveStep(link ++ List(move.head), segments)
    } else {

      if (link.head == link.last && segmentsTail.isEmpty) {
        val appliedBoard = this.applyLinkCopy(link)
        board.getNumberCoords().foreach {
          case (num, x, y) => if (appliedBoard.getSideCount(x, y) != num) {
            //println(s"Expected $num ($x,$y), got ${appliedBoard.getSideCount(x, y)}")
            return None
          }
        }

        return Some(link)
      }


      //      // Needed?
      //      val (sx, sy) = (Math.min(lastMove.head._1, lastMove.last._1), Math.min(lastMove.head._2, lastMove.last._2))
      //      if (sx < width && sy < height) {
      //        val currNum = getNumber(sx, sy)
      //        if (currNum.isDefined) {
      //          val appliedBoard = this.applyLinkCopy(link)
      //          if (appliedBoard.getSideCount(sx, sy) > currNum.get) {
      //            //println(s"Expected ${currNum.get} ($sx,$sy), got ${appliedBoard.getSideCount(sx, sy)}")
      //            return None
      //          }
      //        }
      //      }


      val direction = (lastMove.last._1 - lastMove.head._1, lastMove.last._2 - lastMove.head._2)

      val allPossibleMoves = direction match {
        case (0, -1) => List((cx, cy - 1), (cx + 1, cy), (cx - 1, cy))
        case (1, 0) => List((cx + 1, cy), (cx, cy + 1), (cx, cy - 1))
        case (0, 1) => List((cx, cy + 1), (cx + 1, cy), (cx - 1, cy))
        case (-1, 0) => List((cx - 1, cy), (cx, cy + 1), (cx, cy - 1))
        case _ => List()
      }

      val boundedMoves = allPossibleMoves.filter(m => m._1 >= 0 && m._2 >= 0 && m._1 <= width && m._2 <= height)

      val nonExistingMoves = boundedMoves.filter(m => m == link.head || !link.contains(m))

      val possibleMoves = direction match {
        case (0, -1) => nonExistingMoves.filter(m => {
          val line = getLeft(m._1, m._2 - 1)
          line.contains(true) || line.isEmpty
        })
        case (1, 0) => nonExistingMoves.filter(m => {
          val line = getTop(m._1, m._2)
          line.contains(true) || line.isEmpty
        })
        case (0, 1) => nonExistingMoves.filter(m => {
          val line = getLeft(m._1, m._2)
          line.contains(true) || line.isEmpty
        })
        case (-1, 0) => nonExistingMoves.filter(m => {
          val line = getTop(m._1 - 1, m._2)
          line.contains(true) || line.isEmpty
        })
      }

      val knownMoves = possibleMoves.filter(move => segmentsTail.exists(s =>
        (s.head == move && s.last == link.last) || (s.head == link.last && s.last == move)))

      val (moves, tail) = if (knownMoves.nonEmpty) {
        (knownMoves ++ possibleMoves, segmentsTail.filter(s => !link.contains(s.head) && !link.contains(s.last)))
      } else {
        (possibleMoves, segmentsTail)
      }

      /*println(this.applyLinkCopy(link)) // DEBUG print
      println("" +
        s"Link: $link\n" +
        s"Known moves: $knownMoves\n" +
        s"Final moves: $moves\n" +
        s"Tail: $tail\n" +
        "\n" +
        "\n")
  */
      moves.toStream.map(nextMove => solveStep(link ++ List(nextMove), tail)).collectFirst { case p if p.isDefined => p.get }
    }
  }*/


  // Rules


  /*def applyLinkCopy(link: List[(Int, Int)]) = {
    val p = new Puzzle(width, height, numbers, vertical, horizontal)
    link.sliding(2).foreach(segment => {
      val a = segment.head
      val b = segment.last

      val direction = (b._1 - a._1, b._2 - a._2)
      direction match {
        case (0, -1) => p.setLeft(a._1, a._2 - 1, true)
        case (1, 0) => p.setTop(a._1, a._2, true)
        case (0, 1) => p.setLeft(a._1, a._2, true)
        case (-1, 0) => p.setTop(a._1 - 1, a._2, true)
        case _ => List()
      }
    })

    p
  }*/
}

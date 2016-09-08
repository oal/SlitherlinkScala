class Puzzle(val width: Int,
             val height: Int,
             val numbers: List[List[Option[Int]]],
             var vertical: List[List[Option[Boolean]]],
             var horizontal: List[List[Option[Boolean]]]) {


  def solve(): Puzzle = {
    applyRules()

    println(this)
    //System.exit(1)

    val uniqueSegments = getSolvedSegments().distinct

    val link = uniqueSegments.head

    println("TAIL: ", uniqueSegments.tail)
    val solvedLink = solveStep(link, uniqueSegments.tail).get
    this.applyLinkCopy(solvedLink)
  }

  // Recursive solver
  private def solveStep(link: List[(Int, Int)], segmentsTail: List[List[(Int, Int)]]): Option[List[(Int, Int)]] = {
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
        getNumberCoords().foreach {
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
  }


  // Rules
  def ruleOneInCorner() = {
    getNumberCoords(1).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        setTop(x, y, false)
        setLeft(x, y, false)
      } else if (coord == (width - 1, 0)) {
        setTop(x, y, false)
        setRight(x, y, false)
      } else if (coord == (width - 1, height - 1)) {
        setBottom(x, y, false)
        setRight(x, y, false)
      } else if (coord == (0, height - 1)) {
        setBottom(x, y, false)
        setLeft(x, y, false)
      }
    })
  }

  def ruleTwoInCorner() = {
    getNumberCoords(2).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        setTop(x + 1, y, true)
        setLeft(x, y + 1, true)
      } else if (coord == (width - 1, 0)) {
        setTop(x - 1, y, true)
        setRight(x, y + 1, true)
      } else if (coord == (width - 1, height - 1)) {
        setBottom(x - 1, y, true)
        setRight(x, y - 1, true)
      } else if (coord == (0, height - 1)) {
        setBottom(x + 1, y, true)
        setLeft(x, y - 1, true)
      }
    })
  }

  def ruleThreeInCorner() = {
    getNumberCoords(3).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        setTop(x, y, true)
        setLeft(x, y, true)
      } else if (coord == (width - 1, 0)) {
        setTop(x, y, true)
        setRight(x, y, true)
      } else if (coord == (width - 1, height - 1)) {
        setBottom(x, y, true)
        setRight(x, y, true)
      } else if (coord == (0, height - 1)) {
        setBottom(x, y, true)
        setLeft(x, y, true)
      }
    })
  }

  def ruleAdjacent3s() = {
    getNumberCoords(3).foreach(coord => {
      // Side by side
      val (x, y) = coord
      if (x < width - 1 && getNumber(x + 1, y).contains(3)) {
        setLeft(x, y, true)
        setRight(x, y, true)
        setRight(x + 1, y, true)

        if (y > 0) setRight(x, y - 1, false)
        if (y < height - 1) setRight(x, y + 1, false)
      }

      // Above and beyond
      if (y < height - 1 && getNumber(x, y + 1).contains(3)) {
        setTop(x, y, true)
        setTop(x, y + 1, true)
        setBottom(x, y + 1, true)

        if (x > 0) setBottom(x - 1, y, false)
        if (x < width - 1) setBottom(x + 1, y, false)
      }
    })
  }

  def applyRules() = {
    ruleOneInCorner()
    ruleTwoInCorner()
    ruleThreeInCorner()

    //ruleAdjacent3s()
  }


  // Getters and setters
  def getTop(x: Int, y: Int): Option[Boolean] = {
    if (x < 0 || y < 0 || x > width || y > width) None
    else horizontal(y)(x)
  }

  def getBottom(x: Int, y: Int): Option[Boolean] = getTop(x, y + 1)

  def getLeft(x: Int, y: Int): Option[Boolean] = {
    if (x < 0 || y < 0 || x > width || y > width) None
    else vertical(y)(x)
  }

  def getRight(x: Int, y: Int): Option[Boolean] = getLeft(x + 1, y)

  def getNumber(x: Int, y: Int) = numbers(y)(x)

  // Get any number cell
  def getNumberCoords(): List[(Int, Int, Int)] = {
    numbers.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (num, x) =>
        if (num.isDefined) Some((num.get, x, y)) else None
      }
    }.filter(_.isDefined).map(c => c.get)
  }

  // Get specific number cell
  def getNumberCoords(n: Int): List[(Int, Int)] = {
    numbers.zipWithIndex.flatMap { case (row, y) =>
      row.zipWithIndex.map { case (num, x) =>
        if (num.isDefined && num.get == n) Some((x, y)) else None
      }
    }.filter(_.isDefined).map(c => c.get)
  }

  def getSideCount(x: Int, y: Int) = {
    List(getTop(x, y), getRight(x, y), getBottom(x, y), getLeft(x, y)).count(_.contains(true))
  }

  private def getSolvedSegments(x: Int = 0, y: Int = 0, dir: Int = 0, segments: List[List[(Int, Int)]] = List()): List[List[(Int, Int)]] = {
    if (y >= height) return segments

    val ndir = (dir + 1) % 4
    val (nx, ny) = if (ndir == 0) {
      val nx = if (x == width - 1) 0 else x + 1
      val ny = if (nx > x) y else y + 1
      (nx, ny)
    } else {
      (x, y)
    }

    val segment = ndir match {
      case 0 => if (getTop(x, y).contains(true)) Some(List((x, y), (x + 1, y))) else None
      case 1 => if (getRight(x, y).contains(true)) Some(List((x + 1, y), (x + 1, y + 1))) else None
      case 2 => if (getBottom(x, y).contains(true)) Some(List((x, y + 1), (x + 1, y + 1))) else None
      case 3 => if (getLeft(x, y).contains(true)) Some(List((x, y), (x, y + 1))) else None
      case _ => None
    }

    if (segment.isDefined) segment.get :: getSolvedSegments(nx, ny, ndir) else getSolvedSegments(nx, ny, ndir)
  }

  def setTop(x: Int, y: Int, value: Boolean) = {
    val newRow = horizontal(y).take(x) ++ List(Some(value)) ++ horizontal(y).drop(x + 1)
    horizontal = horizontal.take(y) ++ List(newRow) ++ horizontal.drop(y + 1)
  }

  def setBottom(x: Int, y: Int, value: Boolean) = {
    setTop(x, y + 1, value)
  }

  def setLeft(x: Int, y: Int, value: Boolean) = {
    val newCol = vertical(y).take(x) ++ List(Some(value)) ++ vertical(y).drop(x + 1)
    vertical = vertical.take(y) ++ List(newCol) ++ vertical.drop(y + 1)
  }

  def setRight(x: Int, y: Int, value: Boolean) = {
    setLeft(x + 1, y, value)
  }

  // Stringify
  override def toString: String = {
    val board = (0 until height).map(y => {
      val horiz = (0 until width).map(x => {
        val top = getTop(x, y)
        if (top.isDefined) {
          if (top.get) "-" else " "
        } else {
          " "
        }
      }).mkString("+")
      val verti = (0 to width).map(x => {
        val left = getLeft(x, y)
        if (left.isDefined) {
          if (left.get) "|" else " "
        } else {
          " "
        }
      }).mkString(" ") // to or until?

      s"+$horiz+\n$verti"
    }).mkString("\n")

    val lastHoriz = (0 until width).map(x => {
      val bottom = getBottom(x, height - 1)
      if (bottom.isDefined) {
        if (bottom.get) "-" else " "
      } else {
        " "
      }
    }).mkString("+")

    s"${width}x$height\n$board\n+$lastHoriz+"
  }

  def applyLinkCopy(link: List[(Int, Int)]) = {
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
  }
}

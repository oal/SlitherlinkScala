object Rules {
  private def zero(b: Board) = {
    var board = b
    b.getNumberCoords(0).foreach(coord => {
      val (x, y) = coord
      board = board.setTop(x, y, false).setLeft(x, y, false).setBottom(x, y, false).setRight(x, y, false)

      // Zero along top edge
      if (y == 0) {
        if (x > 0) board = board.setTop(x - 1, y, false)
        if (x < board.width - 1) board = board.setTop(x + 1, y, false)
      }

      // Zero along right edge
      if (x == board.width - 1) {
        if (y > 0) board = board.setRight(x, y - 1, false)
        if (y < board.height - 1) board = board.setRight(x, y + 1, false)
      }

      // Zero along bottom edge
      if (y == board.height - 1) {
        if (x > 0) board = board.setBottom(x - 1, y, false)
        if (x < board.width - 1) board = board.setBottom(x + 1, y, false)
      }

      // Zero along left edge
      if (x == 0) {
        if (y > 0) board = board.setLeft(x, y - 1, false)
        if (y < board.height - 1) board = board.setLeft(x, y + 1, false)
      }
    })
    board
  }

  private def oneInCorner(b: Board) = {
    var board = b
    board.getNumberCoords(1).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        board = board.setTop(x, y, false).setLeft(x, y, false)
      } else if (coord == (board.width - 1, 0)) {
        board = board.setTop(x, y, false).setRight(x, y, false)
      } else if (coord == (board.width - 1, board.height - 1)) {
        board = board.setBottom(x, y, false).setRight(x, y, false)
      } else if (coord == (0, board.height - 1)) {
        board = board.setBottom(x, y, false).setLeft(x, y, false)
      }
    })
    board
  }

  private def twoInCorner(b: Board) = {
    var board = b
    board.getNumberCoords(2).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        board = board.setTop(x + 1, y, true).setLeft(x, y + 1, true)
      } else if (coord == (board.width - 1, 0)) {
        board = board.setTop(x - 1, y, true).setRight(x, y + 1, true)
      } else if (coord == (board.width - 1, board.height - 1)) {
        board = board.setBottom(x - 1, y, true).setRight(x, y - 1, true)
      } else if (coord == (0, board.height - 1)) {
        board = board.setBottom(x + 1, y, true).setLeft(x, y - 1, true)
      }
    })
    board
  }

  private def threeInCorner(b: Board) = {
    var board = b
    board.getNumberCoords(3).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        board = board.setTop(x, y, true).setLeft(x, y, true)
      } else if (coord == (board.width - 1, 0)) {
        board = board.setTop(x, y, true).setRight(x, y, true)
      } else if (coord == (board.width - 1, board.height - 1)) {
        board = board.setBottom(x, y, true).setRight(x, y, true)
      } else if (coord == (0, board.height - 1)) {
        board = board.setBottom(x, y, true).setLeft(x, y, true)
      }
    })
    board
  }

  private def adjacent3s(b: Board) = {
    var board = b
    board.getNumberCoords(3).foreach(coord => {
      // Side by side
      val (x, y) = coord
      if (x < board.width - 1 && board.getNumber(x + 1, y).contains(3)) {
        board = board.setLeft(x, y, true).setRight(x, y, true).setRight(x + 1, y, true).setRight(x, y - 1, false).setRight(x, y + 1, false)
      }

      // Above and beyond
      if (y < board.height - 1 && board.getNumber(x, y + 1).contains(3)) {
        board = board.setTop(x, y, true).setTop(x, y + 1, true).setBottom(x, y + 1, true).setBottom(x - 1, y, false).setBottom(x + 1, y, false)
      }
    })
    board
  }

  private def diagonal3s(b: Board) = {
    var board = b
    board.getNumberCoords(3).foreach(coord => {
      val (x, y) = coord

      // Down to the right
      if (x < board.width - 1 && y < board.height - 1 && board.getNumber(x + 1, y + 1).contains(3)) {
        board =board.setTop(x, y, true).setLeft(x, y, true).setRight(x + 1, y + 1, true).setBottom(x + 1, y + 1, true)
      }

      // Down to the left
      if (x > 0 && y < board.height - 1 && board.getNumber(x - 1, y + 1).contains(3)) {
        board =board.setTop(x, y, true).setRight(x, y, true).setLeft(x - 1, y + 1, true).setBottom(x - 1, y + 1, true)
      }
    })
    board
  }

  private def threeNextToZero(b: Board) = {
    var board = b
    board.getNumberCoords(3).foreach(coord => {
      val (x, y) = coord

      // Zero above three
      if (y > 0 && board.getNumber(x, y - 1).contains(0)) {
        board = board.setTop(x - 1, y, true).setTop(x + 1, y, true).setLeft(x, y, true).setBottom(x, y, true).setRight(x, y, true)
      }

      // Three above zero
      if (y < board.height - 1 && board.getNumber(x, y + 1).contains(0)) {
        board = board.setBottom(x - 1, y, true).setBottom(x + 1, y, true).setLeft(x, y, true).setTop(x, y, true).setRight(x, y, true)
      }

      // Three to right of zero
      if (x > 0 && board.getNumber(x - 1, y).contains(0)) {
        board = board.setLeft(x, y - 1, true).setLeft(x, y + 1, true).setTop(x, y, true).setRight(x, y, true).setBottom(x, y, true)
      }

      // Three to left of zero
      if (x < board.width - 1 && board.getNumber(x + 1, y).contains(0)) {
        board = board.setRight(x, y - 1, true).setRight(x, y + 1, true).setTop(x, y, true).setLeft(x, y, true).setBottom(x, y, true)
      }
    })
    board
  }

  // Apply last
  private def disableOutsideBends(board: Board) = {
    // Whenever an L shape occurs, the places (left, down) from it can never be set. Same for rotations.

    var b = board
    for (y <- 0 until board.height) {
      for (x <- 0 until board.width) {
        if (b.getTop(x, y).contains(true) && b.getRight(x, y).contains(true)) {
          b = b.setLeft(x + 1, y - 1, false).setBottom(x + 1, y - 1, false)
        }

        if (b.getRight(x, y).contains(true) && b.getBottom(x, y).contains(true)) {
          b = b.setTop(x + 1, y + 1, false).setLeft(x + 1, y + 1, false)
        }

        if (b.getBottom(x, y).contains(true) && b.getLeft(x, y).contains(true)) {
          b = b.setTop(x - 1, y + 1, false).setRight(x - 1, y + 1, false)
        }

        if (b.getLeft(x, y).contains(true) && b.getTop(x, y).contains(true)) {
          b = b.setRight(x - 1, y - 1, false).setBottom(x - 1, y - 1, false)
        }
      }
    }
    b
  }

  def applyRules(board: Board): Board = {
    val r1 = zero(board)
    val r2 = oneInCorner(r1)
    val r3 = twoInCorner(r2)
    val r4 = threeInCorner(r3)

    val r5 = adjacent3s(r4)
    val r6 = diagonal3s(r5)
    val r7 = threeNextToZero(r6)

    // Last
    val r8 = disableOutsideBends(r7)
    r8
  }

}

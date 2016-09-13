object Rules {
  private def zero(board: Board) = {
    board.getNumberCoords(0).foreach(coord => {
      val (x, y) = coord
      board.setTop(x, y, false)
      board.setLeft(x, y, false)
      board.setBottom(x, y, false)
      board.setRight(x, y, false)

      // Zero along top edge
      if (y == 0) {
        if (x > 0) board.setTop(x - 1, y, false)
        if (x < board.width - 1) board.setTop(x + 1, y, false)
      }

      // Zero along right edge
      if (x == board.width - 1) {
        if (y > 0) board.setRight(x, y - 1, false)
        if (y < board.height - 1) board.setRight(x, y + 1, false)
      }

      // Zero along bottom edge
      if (y == board.height - 1) {
        if (x > 0) board.setBottom(x - 1, y, false)
        if (x < board.width - 1) board.setBottom(x + 1, y, false)
      }

      // Zero along left edge
      if (x == 0) {
        if (y > 0) board.setLeft(x, y - 1, false)
        if (y < board.height - 1) board.setLeft(x, y + 1, false)
      }
    })
  }

  private def oneInCorner(board: Board) = {
    board.getNumberCoords(1).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        board.setTop(x, y, false)
        board.setLeft(x, y, false)
      } else if (coord == (board.width - 1, 0)) {
        board.setTop(x, y, false)
        board.setRight(x, y, false)
      } else if (coord == (board.width - 1, board.height - 1)) {
        board.setBottom(x, y, false)
        board.setRight(x, y, false)
      } else if (coord == (0, board.height - 1)) {
        board.setBottom(x, y, false)
        board.setLeft(x, y, false)
      }
    })
  }

  private def twoInCorner(board: Board) = {
    board.getNumberCoords(2).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        board.setTop(x + 1, y, true)
        board.setLeft(x, y + 1, true)
      } else if (coord == (board.width - 1, 0)) {
        board.setTop(x - 1, y, true)
        board.setRight(x, y + 1, true)
      } else if (coord == (board.width - 1, board.height - 1)) {
        board.setBottom(x - 1, y, true)
        board.setRight(x, y - 1, true)
      } else if (coord == (0, board.height - 1)) {
        board.setBottom(x + 1, y, true)
        board.setLeft(x, y - 1, true)
      }
    })
  }

  private def threeInCorner(board: Board) = {
    board.getNumberCoords(3).foreach(coord => {
      val (x, y) = coord
      if (coord == (0, 0)) {
        board.setTop(x, y, true)
        board.setLeft(x, y, true)
      } else if (coord == (board.width - 1, 0)) {
        board.setTop(x, y, true)
        board.setRight(x, y, true)
      } else if (coord == (board.width - 1, board.height - 1)) {
        board.setBottom(x, y, true)
        board.setRight(x, y, true)
      } else if (coord == (0, board.height - 1)) {
        board.setBottom(x, y, true)
        board.setLeft(x, y, true)
      }
    })
  }

  private def adjacent3s(board: Board) = {
    board.getNumberCoords(3).foreach(coord => {
      // Side by side
      val (x, y) = coord
      if (x < board.width - 1 && board.getNumber(x + 1, y).contains(3)) {
        board.setLeft(x, y, true)
        board.setRight(x, y, true)
        board.setRight(x + 1, y, true)

        board.setRight(x, y - 1, false)
        board.setRight(x, y + 1, false)
      }

      // Above and beyond
      if (y < board.height - 1 && board.getNumber(x, y + 1).contains(3)) {
        board.setTop(x, y, true)
        board.setTop(x, y + 1, true)
        board.setBottom(x, y + 1, true)

        board.setBottom(x - 1, y, false)
        board.setBottom(x + 1, y, false)
      }
    })
  }

  private def diagonal3s(board: Board) = {
    board.getNumberCoords(3).foreach(coord => {
      val (x, y) = coord

      // Down to the right
      if (x < board.width - 1 && y < board.height - 1 && board.getNumber(x + 1, y + 1).contains(3)) {
        board.setTop(x, y, true)
        board.setLeft(x, y, true)

        board.setRight(x + 1, y + 1, true)
        board.setBottom(x + 1, y + 1, true)
      }

      // Down to the left
      if (x > 0 && y < board.height - 1 && board.getNumber(x - 1, y + 1).contains(3)) {
        board.setTop(x, y, true)
        board.setRight(x, y, true)

        board.setLeft(x - 1, y + 1, true)
        board.setBottom(x - 1, y + 1, true)
      }
    })
  }

  private def threeNextToZero(board: Board) = {
    board.getNumberCoords(3).foreach(coord => {
      val (x, y) = coord

      // Zero above three
      if (y > 0 && board.getNumber(x, y - 1).contains(0)) {
        board.setTop(x - 1, y, true)
        board.setTop(x + 1, y, true)
        board.setLeft(x, y, true)
        board.setBottom(x, y, true)
        board.setRight(x, y, true)
      }

      // Three above zero
      if (y < board.height - 1 && board.getNumber(x, y + 1).contains(0)) {
        board.setBottom(x - 1, y, true)
        board.setBottom(x + 1, y, true)
        board.setLeft(x, y, true)
        board.setTop(x, y, true)
        board.setRight(x, y, true)
      }

      // Three to right of zero
      if (x > 0 && board.getNumber(x - 1, y).contains(0)) {
        board.setLeft(x, y - 1, true)
        board.setLeft(x, y + 1, true)

        board.setTop(x, y, true)
        board.setRight(x, y, true)
        board.setBottom(x, y, true)
      }

      // Three to left of zero
      if (x < board.width - 1 && board.getNumber(x + 1, y).contains(0)) {
        board.setRight(x, y - 1, true)
        board.setRight(x, y + 1, true)

        board.setTop(x, y, true)
        board.setLeft(x, y, true)
        board.setBottom(x, y, true)
      }
    })
  }

  // Apply last
  private def disableOutsideBends(board: Board) = {
    // Whenever an L shape occurs, the places (left, down) from it can never be set. Same for rotations.

    for (y <- 0 until board.height) {
      for (x <- 0 until board.width) {
        if (board.getTop(x, y).contains(true) && board.getRight(x, y).contains(true)) {
          board.setLeft(x + 1, y - 1, false)
          board.setBottom(x + 1, y - 1, false)
        }

        if (board.getRight(x, y).contains(true) && board.getBottom(x, y).contains(true)) {
          board.setTop(x + 1, y + 1, false)
          board.setLeft(x + 1, y + 1, false)
        }

        if (board.getBottom(x, y).contains(true) && board.getLeft(x, y).contains(true)) {
          board.setTop(x - 1, y + 1, false)
          board.setRight(x - 1, y + 1, false)
        }

        if (board.getLeft(x, y).contains(true) && board.getTop(x, y).contains(true)) {
          board.setRight(x - 1, y - 1, false)
          board.setBottom(x - 1, y - 1, false)
        }
      }
    }
  }

  def applyRules(board: Board) = {
    zero(board)
    oneInCorner(board)
    twoInCorner(board)
    threeInCorner(board)

    adjacent3s(board)
    diagonal3s(board)
    threeNextToZero(board)

    // Last
    disableOutsideBends(board)
  }

}

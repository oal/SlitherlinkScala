object Rules {
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

        if (y > 0) board.setRight(x, y - 1, false)
        if (y < board.height - 1) board.setRight(x, y + 1, false)
      }

      // Above and beyond
      if (y < board.height - 1 && board.getNumber(x, y + 1).contains(3)) {
        board.setTop(x, y, true)
        board.setTop(x, y + 1, true)
        board.setBottom(x, y + 1, true)

        if (x > 0) board.setBottom(x - 1, y, false)
        if (x < board.width - 1) board.setBottom(x + 1, y, false)
      }
    })
  }

  def applyRules(board: Board) = {
    oneInCorner(board)
    twoInCorner(board)
    threeInCorner(board)

    adjacent3s(board)
  }

}

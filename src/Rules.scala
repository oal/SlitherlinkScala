object Rules {
  def applyRules(board: Board) = {
    applyThrees(board)
    applyCorners(board)
  }

  def applyCorners(board: Board): Unit = {
    //check top left corner, set connectors according to value
    if (board.row(0).square(0).value == 3) {
      board.setConnector(0, 0, 'Up, s = true, l = true)
      board.setConnector(0, 0, 'Left, s = true, l = true)
    }
    else if (board.row(0).square(0).value == 2) {
      board.setConnector(0, 1, 'Up, s = true, l = true)
      board.setConnector(1, 0, 'Left, s = true, l = true)
    }

    //check top right corner, set connectors according to value
    if (board.row(0).square(board.row(0).square.size - 1).value == 3) {
      board.setConnector(0, board.row(0).square.size - 1, 'Up, s = true, l = true)
      board.setConnector(0, board.row(0).square.size - 1, 'Right, s = true, l = true)
    }
    else if (board.row(0).square(board.row(0).square.size - 1).value == 2) {
      board.setConnector(0, board.row(0).square.size - 2, 'Up, s = true, l = true)
      board.setConnector(1, board.row(0).square.size - 1, 'Right, s = true, l = true)
    }

    //check bottom left corner, set connectors according to value
    if (board.row(board.row.size - 1).square(0).value == 3) {
      board.setConnector(board.row.size - 1, 0, 'Down, s = true, l = true)
      board.setConnector(board.row.size - 1, 0, 'Left, s = true, l = true)
    }
    else if (board.row(board.row.size - 1).square(0).value == 2) {
      board.setConnector(board.row.size - 1, 1, 'Down, s = true, l = true)
      board.setConnector(board.row.size - 2, 0, 'Left, s = true, l = true)
    }

    //check bottom right corner, set connectors according to value
    if (board.row(board.row.size - 1).square(board.row(0).square.size - 1).value == 3) {
      board.setConnector(board.row.size - 1, board.row(0).square.size - 1, 'Down, s = true, l = true)
      board.setConnector(board.row.size - 1, board.row(0).square.size - 1, 'Right, s = true, l = true)
    }
    else if (board.row(board.row.size - 1).square(board.row(0).square.size - 1).value == 2) {
      board.setConnector(board.row.size - 1, board.row(0).square.size - 2, 'Down, s = true, l = true)
      board.setConnector(board.row.size - 2, board.row(0).square.size - 1, 'Right, s = true, l = true)
    }
  }

  def applyThrees(b: Board) {
    for (y <- b.row) {
      for (x <- y.square) {
        if (x.value == 3) {
          //Tries to find any value zero adjacent to 3 and sets connectors according to direction
          if (x.y > 0 && b.row(x.y - 1).square(x.x).value == 0) {
            //Tries to find a zero above the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y - 1, x.x + 1, 'Down, s = true, l = true) //Sets the bottom connector of the square in the "northeast" diagonal of the original
            b.setConnector(x.y - 1, x.x - 1, 'Down, s = true, l = true) //Sets the bottom connector of the square in the "northwest" diagonal of the original
          }
          else if (x.y < b.row.size - 1 && b.row(x.y + 1).square(x.x).value == 0) {
            //Tries to find a zero below the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y + 1, x.x + 1, 'Up, s = true, l = true) //Sets the top connector of the square in the "southeast" diagonal of the original
            b.setConnector(x.y + 1, x.x - 1, 'Up, s = true, l = true) //Sets the top connector of the square in the "southwest" diagonal of the original
          }
          else if (x.x > 0 && b.row(x.y).square(x.x - 1).value == 0) {
            //Tries to find a zero left of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y - 1, x.x - 1, 'Right, s = true, l = true) //Sets the right connector of the square in the "northwest" diagonal of the original
            b.setConnector(x.y + 1, x.x - 1, 'Right, s = true, l = true) //Sets the right connector of the square in the "southwest" diagonal of the original
          }
          else if (x.x < b.row(x.y).square.size - 1
            && b.row(x.y).square(x.x + 1).value == 0) {
            //Tries to find a zero right of the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y - 1, x.x + 1, 'Left, s = true, l = true) //Sets the right connector of the square in the "northeast" diagonal of the original
            b.setConnector(x.y + 1, x.x + 1, 'Left, s = true, l = true) //Sets the right connector of the square in the "southeast" diagonal of the original
          }

          if (x.y > 0 && x.x > 0 && b.row(x.y - 1).square(x.x - 1).value == 0) {
            //Tries to find a zero "northwest" of the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
          }

          if (x.y > 0 && x.x < b.row(x.y).square.size - 1
            && b.row(x.y - 1).square(x.x + 1).value == 0) {
            //Tries to find a zero "northeast" of the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
          }

          if (x.y < b.row.size - 1 && x.x > 0
            && b.row(x.y + 1).square(x.x - 1).value == 0) {
            //Tries to find a zero "southwest" of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
          }

          if (x.y < b.row.size - 1 && x.x < b.row(x.y).square.size - 1
            && b.row(x.y + 1).square(x.x + 1).value == 0) {
            //Tries to find a zero "southeast" of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
          }

          //Tries to find any value zero adjacent to 3 and sets connectors according to direction
          if (x.y > 0 && b.row(x.y - 1).square(x.x).value == 3) {
            //Tries to find a three above the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y - 1, x.x, 'Up, s = true, l = true) //Sets the bottom connector of the adjacent square
          }

          if (x.y < b.row.size - 1 && b.row(x.y + 1).square(x.x).value == 3) {
            //Tries to find a three below the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y + 1, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the adjacent square
          }

          if (x.x > 0 && b.row(x.y).square(x.x - 1).value == 3) {
            //Tries to find a three left of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
            b.setConnector(x.y, x.x - 1, 'Left, s = true, l = true) //Sets the left connector of the adjacent square
          }

          if (x.x < b.row(x.y).square.size - 1
            && b.row(x.y).square(x.x + 1).value == 3) {
            //Tries to find a three right of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
            b.setConnector(x.y, x.x + 1, 'Right, s = true, l = true) //Sets the right connector of the adjacent square
          }
        }
      }
    }
  }
}

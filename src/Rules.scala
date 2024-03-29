object Rules {
  // These are applied once, and do not depend on other rules
  def applyRules(board: Board) = {
    apply1x1(board)
    if (board.row.length > 1) {
      applyThrees(board)
      applyCorners(board)
    }
  }

  def apply1x1(board: Board) = {
    if (board.row.length == 1) {
      board.setConnector(0, 0, 'Up, s = true, l = true)
      board.setConnector(0, 0, 'Right, s = true, l = true)
      board.setConnector(0, 0, 'Down, s = true, l = true)
      board.setConnector(0, 0, 'Left, s = true, l = true)
    }
  }

  def applyCorners(board: Board): Unit = {
    //check top left corner, set connectors according to value
    if (board.getSquare(0, 0).value == 3) {
      board.setConnector(0, 0, 'Up, s = true, l = true)
      board.setConnector(0, 0, 'Left, s = true, l = true)
    }
    else if (board.getSquare(0, 0).value == 2) {
      board.setConnector(0, 1, 'Up, s = true, l = true)
      board.setConnector(1, 0, 'Left, s = true, l = true)
    }

    // Ones left and below top left corner square
    if (board.getSquare(1, 0).value == 1 && board.getSquare(0, 1).value == 1) {
      board.setConnector(0, 0, 'Right, s = false, l = true)
      board.setConnector(0, 0, 'Down, s = false, l = true)
    }

    // Same as above, but with 3 and 1 instead of 1 and 1, both variants
    if (board.getSquare(1, 0).value == 3 && board.getSquare(0, 1).value == 1) {
      board.setConnector(0, 0, 'Right, s = true, l = true)
      board.setConnector(0, 0, 'Down, s = false, l = true)
    } else if (board.getSquare(1, 0).value == 1 && board.getSquare(0, 1).value == 3) {
      board.setConnector(0, 0, 'Right, s = false, l = true)
      board.setConnector(0, 0, 'Down, s = true, l = true)
    }

    // Same as above, but with 3 and 2
    if (board.getSquare(1, 0).value == 3 && board.getSquare(0, 1).value == 2) {
      board.setConnector(0, 1, 'Right, s = true, l = true)
    } else if (board.getSquare(1, 0).value == 2 && board.getSquare(0, 1).value == 3) {
      board.setConnector(1, 0, 'Down, s = true, l = true)
    }

    //---------

    //check top right corner, set connectors according to value
    val rightmost = board.row(0).square.size - 1;
    if (board.getSquare(rightmost, 0).value == 3) {
      board.setConnector(0, rightmost, 'Up, s = true, l = true)
      board.setConnector(0, rightmost, 'Right, s = true, l = true)
    }
    else if (board.row(0).square(board.row(0).square.size - 1).value == 2) {
      board.setConnector(0, rightmost - 1, 'Up, s = true, l = true)
      board.setConnector(1, rightmost, 'Right, s = true, l = true)
    }

    // Ones right and below top right corner square
    if (board.getSquare(rightmost - 1, 0).value == 1 && board.getSquare(rightmost, 1).value == 1) {
      board.setConnector(0, rightmost, 'Left, s = false, l = true)
      board.setConnector(0, rightmost, 'Down, s = false, l = true)
    }

    // Same as above, but with 3 and 1 instead of 1 and 1, both variants
    if (board.getSquare(rightmost - 1, 0).value == 3 && board.getSquare(rightmost, 1).value == 1) {
      board.setConnector(0, rightmost, 'Left, s = true, l = true)
      board.setConnector(0, rightmost, 'Down, s = false, l = true)
    } else if (board.getSquare(rightmost - 1, 0).value == 1 && board.getSquare(rightmost, 1).value == 3) {
      board.setConnector(0, rightmost, 'Left, s = false, l = true)
      board.setConnector(0, rightmost, 'Down, s = true, l = true)
    }

    // Same as above, but with 3 and 2
    if (board.getSquare(rightmost - 1, 0).value == 3 && board.getSquare(rightmost, 1).value == 2) {
      board.setConnector(0, rightmost-1, 'Left, s = true, l = true)
    } else if (board.getSquare(rightmost - 1, 0).value == 2 && board.getSquare(rightmost, 1).value == 3) {
      board.setConnector(1, rightmost, 'Down, s = true, l = true)
    }

    //---------

    //check bottom left corner, set connectors according to value
    val bottommost = board.row.size - 1
    if (board.getSquare(0, bottommost).value == 3) {
      board.setConnector(bottommost, 0, 'Down, s = true, l = true)
      board.setConnector(bottommost, 0, 'Left, s = true, l = true)
    }
    else if (board.getSquare(0, bottommost).value == 2) {
      board.setConnector(bottommost, 1, 'Down, s = true, l = true)
      board.setConnector(bottommost-1, 0, 'Left, s = true, l = true)
    }

    // Ones right and above bottom left corner square
    if (board.getSquare(0, bottommost - 1).value == 1 && board.getSquare(1, bottommost).value == 1) {
      board.setConnector(bottommost, 0, 'Right, s = false, l = true)
      board.setConnector(bottommost, 0, 'Up, s = false, l = true)
    }

    // Same as above, but with 3 and 1 instead of 1 and 1, both variants
    if (board.getSquare(0, bottommost - 1).value == 3 && board.getSquare(1, bottommost).value == 1) {
      board.setConnector(bottommost, 0, 'Right, s = false, l = true)
      board.setConnector(bottommost, 0, 'Up, s = true, l = true)
    } else if (board.getSquare(0, bottommost - 1).value == 1 && board.getSquare(1, bottommost).value == 3) {
      board.setConnector(bottommost, 0, 'Right, s = true, l = true)
      board.setConnector(bottommost, 0, 'Up, s = false, l = true)
    }

    // Same as above, but with 3 and 2
    if (board.getSquare(0, bottommost - 1).value == 3 && board.getSquare(1, bottommost).value == 2) {
      board.setConnector(bottommost-1, 0, 'Up, s = true, l = true)
    } else if (board.getSquare(0, bottommost - 1).value == 2 && board.getSquare(1, bottommost).value == 3) {
      board.setConnector(bottommost, 1, 'Right, s = true, l = true)
    }

    //---------

    //check bottom right corner, set connectors according to value
    if (board.getSquare(rightmost, bottommost).value == 3) {
      board.setConnector(bottommost, rightmost, 'Down, s = true, l = true)
      board.setConnector(bottommost, rightmost, 'Right, s = true, l = true)
    } else if (board.getSquare(rightmost, bottommost).value == 2) {
      board.setConnector(bottommost, rightmost - 1, 'Down, s = true, l = true)
      board.setConnector(bottommost - 1, rightmost, 'Right, s = true, l = true)
    }

    // Ones left and above bottom right corner square
    if (board.getSquare(rightmost, bottommost - 1).value == 1 && board.getSquare(rightmost - 1, bottommost).value == 1) {
      board.setConnector(bottommost, rightmost, 'Left, s = false, l = true)
      board.setConnector(bottommost, rightmost, 'Up, s = false, l = true)
    }

    // Same as above, but with 3 and 1 instead of 1 and 1, both variants
    if (board.getSquare(rightmost, bottommost - 1).value == 3 && board.getSquare(rightmost - 1, bottommost).value == 1) {
      board.setConnector(bottommost, rightmost, 'Left, s = false, l = true)
      board.setConnector(bottommost, rightmost, 'Up, s = true, l = true)
    } else if (board.getSquare(rightmost, bottommost - 1).value == 1 && board.getSquare(rightmost - 1, bottommost).value == 3) {
      board.setConnector(bottommost, rightmost, 'Left, s = true, l = true)
      board.setConnector(bottommost, rightmost, 'Up, s = false, l = true)
    }

    // Same as above, but with 3 and 2
    if (board.getSquare(rightmost, bottommost - 1).value == 3 && board.getSquare(rightmost - 1, bottommost).value == 2) {
      board.setConnector(bottommost-1, rightmost, 'Up, s = true, l = true)
    } else if (board.getSquare(rightmost, bottommost - 1).value == 2 && board.getSquare(rightmost - 1, bottommost).value == 3) {
      board.setConnector(bottommost, rightmost-1, 'Left, s = true, l = true)
    }

    //---------

  }

  def applyThrees(b: Board) {
    for (y <- b.row) {
      for (x <- y.square) {
        if (x.value == 3) {
          //Tries to find any value zero adjacent to 3 and sets connectors according to direction
          if (x.x > 0 && x.y > 0 && b.row(x.y - 1).square(x.x).value == 0) {
            //Tries to find a zero above the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y - 1, x.x + 1, 'Down, s = true, l = true) //Sets the bottom connector of the square in the "northeast" diagonal of the original
            b.setConnector(x.y - 1, x.x - 1, 'Down, s = true, l = true) //Sets the bottom connector of the square in the "northwest" diagonal of the original
          }
          else if (x.x > 0 && x.y > 0 && x.y < b.row.size - 1 && b.row(x.y + 1).square(x.x).value == 0) {
            //Tries to find a zero below the square
            b.setConnector(x.y, x.x, 'Left, s = true, l = true) //Sets the left connector of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y + 1, x.x + 1, 'Up, s = true, l = true) //Sets the top connector of the square in the "southeast" diagonal of the original
            b.setConnector(x.y + 1, x.x - 1, 'Up, s = true, l = true) //Sets the top connector of the square in the "southwest" diagonal of the original
          }
          else if (x.x > 0 && x.y > 0 && b.row(x.y).square(x.x - 1).value == 0) {
            //Tries to find a zero left of the square
            b.setConnector(x.y, x.x, 'Right, s = true, l = true) //Sets the right connector of the square
            b.setConnector(x.y, x.x, 'Down, s = true, l = true) //Sets the bottom connector of the square
            b.setConnector(x.y, x.x, 'Up, s = true, l = true) //Sets the top connector of the square
            b.setConnector(x.y - 1, x.x - 1, 'Right, s = true, l = true) //Sets the right connector of the square in the "northwest" diagonal of the original
            b.setConnector(x.y + 1, x.x - 1, 'Right, s = true, l = true) //Sets the right connector of the square in the "southwest" diagonal of the original
          }
          else if (x.x > 0 && x.y > 0 && x.x < b.row(x.y).square.size - 1
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

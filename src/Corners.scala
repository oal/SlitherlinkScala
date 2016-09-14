/**
  * Created by Torsvik on 06/09/2016.
  */

class Corners(boards: BoardParser, i: Int) {
  private val board = boards.boards(i)

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

  def getBoard = boards
}

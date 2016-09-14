/**
  * Created by Torsvik on 06/09/2016.
  */

class Corners(boards:Boards, i:Int) {
  private val b = boards.board(i)

  //check top left corner, set connectors according to value
  if( b.row(0).square(0).value == 3 ) {
    boards.setConnector(i, 0, 0, "Up", s = true, l = true)
    boards.setConnector(i, 0, 0, "Left", s = true, l = true)
  }
  else if( b.row(0).square(0).value == 2 ) {
    boards.setConnector(i, 0, 1, "Up", s = true, l = true)
    boards.setConnector(i, 1, 0, "Left", s = true, l = true)
  }

  //check top right corner, set connectors according to value
  if( b.row(0).square(b.row(0).square.size - 1).value == 3 ){
    boards.setConnector(i, 0, b.row(0).square.size - 1, "Up", s = true, l = true)
    boards.setConnector(i, 0, b.row(0).square.size - 1, "Right", s = true, l = true)
  }
  else if( b.row(0).square(b.row(0).square.size - 1).value == 2) {
    boards.setConnector(i, 0, b.row(0).square.size - 2, "Up", s = true, l = true)
    boards.setConnector(i, 1, b.row(0).square.size - 1, "Right", s = true, l = true)
  }

  //check bottom left corner, set connectors according to value
  if( b.row(b.row.size - 1).square(0).value == 3 ) {
    boards.setConnector(i, b.row.size - 1, 0, "Down", s = true, l = true)
    boards.setConnector(i, b.row.size - 1, 0, "Left", s = true, l = true)
  }
  else if( b.row(b.row.size - 1).square(0).value == 2) {
    boards.setConnector(i, b.row.size - 1, 1, "Down", s = true, l = true)
    boards.setConnector(i, b.row.size - 2, 0, "Left", s = true, l = true)
  }

  //check bottom right corner, set connectors according to value
  if( b.row(b.row.size - 1).square(b.row(0).square.size - 1).value == 3 ){
    boards.setConnector(i, b.row.size - 1, b.row(0).square.size - 1, "Down", s = true, l = true)
    boards.setConnector(i, b.row.size - 1, b.row(0).square.size - 1, "Right", s = true, l = true)
  }
  else if( b.row(b.row.size - 1).square(b.row(0).square.size - 1).value == 2) {
    boards.setConnector(i, b.row.size - 1, b.row(0).square.size - 2, "Down", s = true, l = true)
    boards.setConnector(i, b.row.size - 2, b.row(0).square.size - 1, "Right", s = true, l = true)
  }

  def getBoard = boards
}

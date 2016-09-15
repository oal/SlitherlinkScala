class SolutionPrinter(board: Board) {
  for (y <- board.row) {
    for (x <- y.square) {
      print("+" + getHorSymbol(x.y, x.x, 'Up))
    }
    println("+")
    for (x <- y.square) {
      print(getVerSymbol(x.y, x.x, 'Left) + " ")
    }
    println(getVerSymbol(y.square.last.y, y.square.last.x, 'Right))
  }
  for (x <- board.row.last.square) {
    print("+" + getHorSymbol(x.y, x.x, 'Down))
  }
  println("+")

  def getHorSymbol(y: Int, x: Int, dir: Symbol): String = {
    if (board.row(y).square(x).connector(dir).set)
      "-"
    else
      " "
  }

  def getVerSymbol(y: Int, x: Int, dir: Symbol): String = {
    if (board.row(y).square(x).connector(dir).set)
      "|"
    else
      " "
  }

}

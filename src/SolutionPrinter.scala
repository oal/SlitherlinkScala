/**
  * Created by Torsvik on 07/09/2016.
  */

class SolutionPrinter(boards: BoardParser, i: Int) {
  private def b = boards.board(i)

  for (y <- b.row) {
    for (x <- y.square) {
      print("+" + getHorSymbol(x.y, x.x, 'Up))
    }
    println("+")
    for (x <- y.square) {
      print(getVerSymbol(x.y, x.x, 'Left) + " ")
    }
    println(getVerSymbol(y.square.last.y, y.square.last.x, 'Right))
  }
  for (x <- b.row.last.square) {
    print("+" + getHorSymbol(x.y, x.x, 'Down))
  }
  println("+")

  def getHorSymbol(y: Int, x: Int, dir: Symbol): String = {
    if (b.row(y).square(x).connector(dir).set)
      "-"
    else
      " "
  }

  def getVerSymbol(y: Int, x: Int, dir: Symbol): String = {
    if (b.row(y).square(x).connector(dir).set)
      "|"
    else
      " "
  }

}

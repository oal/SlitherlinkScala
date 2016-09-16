import java.io.PrintWriter

object SolutionPrinter {
  def printBoard(board: Board, out: PrintWriter) = {
    out.println(s"${board.row.head.square.length}x${board.row.length}")
    for (y <- board.row) {
      for (x <- y.square) {
        out.print("+" + getHorSymbol(x.y, x.x, 'Up))
      }
      out.println("+")
      for (x <- y.square) {
        out.print(getVerSymbol(x.y, x.x, 'Left) + " ")
      }
      out.println(getVerSymbol(y.square.last.y, y.square.last.x, 'Right))
    }
    for (x <- board.row.last.square) {
      out.print("+" + getHorSymbol(x.y, x.x, 'Down))
    }
    out.println("+")

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
}

import java.util.Calendar

/**
  * Created by Torsvik on 05/09/2016.
  */

object Runner extends App {

  val parser = new BoardParser("puzzles/puzzle1")

  parser.boards.foreach(board => {
    val start = Calendar.getInstance().getTimeInMillis
    new Solver(board).solve()
    new SolutionPrinter(board)
    println(Calendar.getInstance().getTimeInMillis - start)
  })
}
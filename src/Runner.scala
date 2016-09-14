import java.util.Calendar

/**
  * Created by Torsvik on 05/09/2016.
  */

object Runner extends App {

  val start = Calendar.getInstance().getTimeInMillis
  val b = new BoardParser("puzzles/puzzle1")
  val sb = new Solver(b, 1).getBoards
  println(Calendar.getInstance().getTimeInMillis - start)

}
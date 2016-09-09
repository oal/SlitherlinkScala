/**
  * Created by Torsvik on 05/09/2016.
  */

object Runner extends App{

  val b = new Boards("puzzles/puzzle1")

  val sb = new Solver(b, 1).getBoards
  val solutionPrinter = new SolutionPrinter(sb, 1)

}
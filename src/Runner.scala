/**
  * Created by Torsvik on 05/09/2016.
  */

object Runner extends App{

  val b = new Boards("puzzles/puzzle1")

  val sb = new Solver(b, 1).getBoard

  for( y <- sb.board(0).row){
    for( x <- y.square){
      print(x.connector("Left").set + " \t")
    }
    println("")
  }

  val sp = new SolutionPrinter(sb, 1)

}
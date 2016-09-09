import java.io.PrintWriter
import java.time.LocalTime


object RunApp extends App {
  val inputdir = "input"
  // Change to real input dir
  val outputdir = "output" // Change to real output dir

  import java.io.File

  val dir = new File(inputdir)
  for (f <- dir.listFiles()) {
    solveSlitherLinks(f)
  }

  def solveSlitherLinks(f: File): Unit = {
    println(f.getName)
    val lines = scala.io.Source.fromFile(f).mkString.split("\n").toList
    val numPuzzles = lines.head


    val out = new PrintWriter(new File(outputdir + "/" + f.getName), "UTF-8")
    out.print(numPuzzles + "\n")

    val boards = Solver.parseBoards(lines.tail)
    boards.foreach(b => {
      val now = LocalTime.now()
      val solution = b.solve()
      println(s"Solved in ${(LocalTime.now().toNanoOfDay - now.toNanoOfDay) / 1000000000.0} seconds.")
      out.print(solution)
    })

    out.close()
  }
}

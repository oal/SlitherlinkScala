import java.io.PrintWriter




object RunApp extends App {
  val inputdir = "Input"
  // Change to real input dir
  val outputdir = "Output" // Change to real output dir

  import java.io.File

  val dir = new File(inputdir)
  for (f <- dir.listFiles()) {
    solveSlitherLinks(f)
  }




  def solveSlitherLinks(f: File): Unit = {
    println(f.getName)
    val lines = scala.io.Source.fromFile(f).mkString.split("\n").toList
    val numPuzzles = lines.head

    println(Solver.parseBoards(lines.tail))

    val out = new PrintWriter(new File(outputdir + "/" + f.getName), "UTF-8")
    out.print(numPuzzles + "\n")

    out.print("5x5\n")
    out.print("+-+-+-+-+-+\n")
    out.print("|         |\n")
    out.print("+-+-+-+ + +\n")
    out.print("      |   |\n")
    out.print("+-+ +-+ +-+\n")
    out.print("| | |   |  \n")
    out.print("+ +-+ +-+ +\n")
    out.print("|     |    \n")
    out.print("+ +-+ +-+-+\n")
    out.print("| | |     |\n")
    out.print("+-+ +-+-+-+\n")
    out.print("7x7\n")
    out.print("+-+ +-+-+-+ +-+\n")
    out.print("| | |     | | |\n")
    out.print("+ +-+ +-+-+ + +\n")
    out.print("|     |     | |\n")
    out.print("+ +-+ +-+ + + +\n")
    out.print("| | |   |   | |\n")
    out.print("+ + +-+ +-+-+ +\n")
    out.print("| |   |       |\n")
    out.print("+ +-+ +-+ +-+-+\n")
    out.print("|   |   | |    \n")
    out.print("+-+ + +-+ +-+-+\n")
    out.print("  | | |       |\n")
    out.print("+-+ + +-+ +-+ +\n")
    out.print("|   |   | | | |\n")
    out.print("+-+-+ + +-+ +-+\n")

    out.close()
  }
}

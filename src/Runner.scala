import java.io.{File, PrintWriter}
import java.util.Calendar

object Runner extends App {
  val dir = new File("input")
  dir.listFiles().filter(file => file.getName.head != '.').foreach(file => {
    println(s"Loading boards from ${file.getAbsolutePath}")
    val parser = new BoardParser(file)

    val out = new PrintWriter(new File(s"output/${file.getName}"), "UTF-8")
    out.println(parser.numPuzzles)

    println(s"Found ${parser.boards.length} boards. Solving...")
    parser.boards.foreach(board => {
      val start = Calendar.getInstance().getTimeInMillis
      new Solver(board).solve()
      SolutionPrinter.printBoard(board, out)
      println(s"Solved ${board.row.length}x${board.row.length} board in ${Calendar.getInstance().getTimeInMillis - start}ms")
    })

    out.close()
  })

}
import java.io.File
import java.util.Calendar

object Runner extends App {
  val dir = new File("input")
  dir.listFiles().foreach(file => {
    println(s"Loading boards from ${file.getAbsolutePath()}")
    val parser = new BoardParser(file)

    println(s"Found ${parser.boards.length} boards. Solving...")
    parser.boards.foreach(board => {
      val start = Calendar.getInstance().getTimeInMillis
      new Solver(board).solve()
      new SolutionPrinter(board)
      println(s"Solved ${board.row.length}x${board.row.length} board in ${Calendar.getInstance().getTimeInMillis - start}ms")
    })
  })

}
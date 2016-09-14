import scala.io.Source

class BoardParser(path: String) {
  private val boardFile = Source.fromFile(path).getLines.toList

  // Tuples of start position of board, and side length
  private val startList = boardFile.zipWithIndex.map({ case (line, i) =>
    if(line.contains("x")) {
      Some(Tuple2(i+1, line.split("x").head.toInt))
    } else None
  }).filter(_.isDefined)

  // Create Board instances from lines
  val numPuzzles = boardFile.head.toInt
  val boards = (0 until numPuzzles).map(i => new Board(boardFile, startList(i).get._1, startList(i).get._2))
}
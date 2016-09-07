/**
  * Created by Torsvik on 05/09/2016.
  */

import scala.io.Source

class Boards(path:String) {
  private val boardFile = Source.fromFile(path).getLines.toList
  private val startList = for (i <- boardFile if i.contains("x"))
                          yield Tuple2(boardFile.indexOf(i) + 1, i.split("x").head.toInt)
  val getBoards = boardFile
  val board = for (i <- 1 to boardFile.head.toInt)
              yield new board(boardFile, startList(i - 1))

  class board(boardfile: List[String], start: (Int, Int)) {
    val getBoard = for (i <- start._1 until start._2 + start._1) yield boardfile(i)
    println(start._2 + "x" + start._2)
    val row = for (i <- start._1 until start._2 + start._1) yield new row(boardfile(i), i-start._1)
  }

  class row(row: String, rownumber: Int) {
    val getRow = row
    val square = for (i <- row.split(" ").indices) yield new square(i, rownumber, stringToInt(row.split(" ")(i)))
    println("")
    private def stringToInt(s: String): Int = if (s == "*") -1 else s.toInt
  }

  class square(nx:Int, ny:Int, nvalue:Int) {
    val value = nvalue
    val x = nx
    val y = ny

    print("( " + x + ", " + y + " )")

    val connector = Map[String, Connector](
      ("Up", new Connector(false)),
      ("Down", new Connector(false)),
      ("Left", new Connector(false)),
      ("Right", new Connector(false))
    )
  }

  class Connector(nset:Boolean){
    var set = nset
    var locked = false
  }

  def setConnector(b:Int, y:Int, x:Int, p:String, s:Boolean) = {
    board(b).row(y).square(x).connector(p).set = s
    p match {
      case "Up" => if( y > 0 ) board(b).row(y-1).square(x).connector("Down").set = s
      case "Down" => if( y < board(b).row.size - 1 ) board(b).row(y+1).square(x).connector("Up").set = s
      case "Left" => if( x > 0) board(b).row(y).square(x-1).connector("Right").set = s
      case "Right" => if( x < board(b).row(y).square.size - 1 ) board(b).row(y).square(x+1).connector("Left").set = s
      case _ => board(b).row(y).square(x).connector.filterKeys(Set("Up","Down","Left","Right"))
    }
  }

}
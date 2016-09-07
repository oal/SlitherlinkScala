/**
  * Created by Torsvik on 07/09/2016.
  */

class SolutionPrinter(boards:Boards, i:Int) {
  private def b = boards.board(i)

  for(y <- b.row){
    for(x <- y.square){
      print("+\t" +getHorSymbol(x.y, x.x, "Up") + "\t")
    }
    println("+")
    for(x <- y.square){
      print(getVerSymbol(x.y, x.x, "Left") + "\t" + x.value + "\t")
    }
    println(getVerSymbol(y.square.last.y, y.square.last.x, "Right"))
  }
  for(x <- b.row.last.square){
    print("+\t" +getHorSymbol(x.y, x.x, "Down") + "\t")
  }
  println("+")

  def getHorSymbol(y:Int, x:Int, dir:String):String = {
    if(b.row(y).square(x).connector(dir).set)
      "-"
    else
      " "
  }

  def getVerSymbol(y:Int, x:Int, dir:String):String = {
    if(b.row(y).square(x).connector(dir).set)
      "|"
    else
      " "
  }

}

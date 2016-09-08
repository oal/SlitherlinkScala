/**
  * Created by Torsvik on 06/09/2016.
  */

class Solver(boards:Boards, i:Int) {
  //val getBoard = new Corners(new Threes(boards, i).getBoard, i).getBoard
  private def b = boards.board(i)
  val dots = new Dots

  class Dots{
    val x = for ( x <- 0 to b.row(0).square.size) yield new x(x)
  }

  class x(x:Int){
    val y = for ( y <- 0 to b.row.size) yield new Dot(x, y)
  }

  class Dot(xPos:Int, yPos:Int){
    val x = xPos
    val y = yPos
    val links = new Links(null, null)

    def setLink(dot: Dot) ={
      if(links.d1 == null)
        links.d1 = dot
      else if(links.d2 == null)
        links.d2 = dot
    }
  }

  class Links(dot1:Dot, dot2:Dot){
    var d1 = dot1
    var d2 = dot2
  }

  def move(dot:Dot, previousDot:Dot):Unit = {
    if(validMove(dot, "Up")){
      makeConnection(dot, dots.x(dot.x).y(dot.y-1))
      move(dots.x(dot.x).y(dot.y-1), dot)
    }
    if(validMove(dot, "Right")){
      makeConnection(dot, dots.x(dot.x+1).y(dot.y))
      move(dots.x(dot.x+1).y(dot.y), dot)
    }
    if(validMove(dot, "Down")){
      makeConnection(dot, dots.x(dot.x).y(dot.y+1))
      move(dots.x(dot.x).y(dot.y+1), dot)
    }
    if(validMove(dot, "Left")){
      makeConnection(dot, dots.x(dot.x).y(dot.y+1))
      move(dots.x(dot.x).y(dot.y+1), dot)
    }
    if(!solved){
      removeConnection(dot, previousDot)
    }
  }

  def validMove(d:Dot, s:String):Boolean = {

  }

  def makeConnection(dot:Dot, toDot:Dot) = {

  }

  def removeConnection(dot:Dot, prevDot:Dot) = {

  }

  def solved():Boolean = {

  }

  /*
  def squareCheck():List[boards.square] = {
    val incompleteSquares = for(y <- boards.board(i).row){
      for(x <- y.square if x.value != -1 && x.isFull()) yield x
    }
  }
  */
}

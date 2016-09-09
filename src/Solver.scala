/**
  * Created by Torsvik on 06/09/2016.
  */

class Solver(boards:Boards, i:Int) {
  private def b = boards.board(i)
  def getBoards = boards
  val dots = new Dots

  start(0,0)
  val sp = new SolutionPrinter(boards, i)

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
  }

  class Links(dot1:Dot, dot2:Dot){
    var d1 = dot1
    var d2 = dot2

    def full:Boolean = d1 != null && d2 != null
    def add(d:Dot) = if(d1 == null) d1 = d else if(d2 == null) d2 = d
    def contains(d:Dot) = d1 == d || d2 == d
    def remove(d:Dot) = if(d1 == d) d1 = null else if(d2 == d) d2 = null
  }

  //TODO find startingpoint
  def start(x:Int, y:Int) = {
    move(dots.x(x).y(y), null)
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
      makeConnection(dot, dots.x(dot.x-1).y(dot.y))
      move(dots.x(dot.x-1).y(dot.y), dot)
    }
    if(!solved){
      removeConnection(dot, previousDot)
    }
  }

  def validMove(d:Dot, s:String):Boolean = {
    if(d.links.full)
      false
    else if(s == "Up"){
      if(d.y > 0 && !dots.x(d.x).y(d.y-1).links.full && !dots.x(d.x).y(d.y-1).links.contains(d)) {
        if (d.x == 0 && !b.row(d.y-1).square(d.x).isFull)
          true
        else if(d.x == b.row(0).square.size && !b.row(d.y-1).square(d.x-1).isFull)
          true
        else if (d.x > 0 && d.x < b.row(0).square.size && !b.row(d.y-1).square(d.x-1).isFull
          && !b.row(d.y-1).square(d.x).isFull)
          true
        else
          false
      }
      else
        false
    }
    else if(s == "Right"){
      if(d.x < dots.x.size-1 && !dots.x(d.x+1).y(d.y).links.full && !dots.x(d.x+1).y(d.y).links.contains(d)){
        if (d.y == 0 && !b.row(d.y).square(d.x).isFull)
          true
        else if(d.y == b.row.size && !b.row(d.y-1).square(d.x).isFull)
          true
        else if(b.row.size > d.y && d.y > 0 && !b.row(d.y).square(d.x).isFull
          && !b.row(d.y-1).square(d.x).isFull)
          true
        else
          false
      }
      else
        false
    }
    else if(s == "Down"){
      if(d.y < dots.x(d.x).y.size-1 && !dots.x(d.x).y(d.y+1).links.full && !dots.x(d.x).y(d.y+1).links.contains(d)){
        if(d.x == 0 && !b.row(d.y).square(d.x).isFull)
          true
        else if(d.x == b.row(0).square.size && !b.row(d.y).square(d.x-1).isFull)
          true
        else if(d.x > 0 && d.x < b.row(d.y).square.size && !b.row(d.y).square(d.x).isFull
          && !b.row(d.y).square(d.x-1).isFull)
          true
        else
          false
      }
      else
        false
    }
    else if(s == "Left"){
      if(d.x > 0 && !dots.x(d.x-1).y(d.y).links.full && !dots.x(d.x-1).y(d.y).links.contains(d)){
        if(d.y == 0 && !b.row(d.y).square(d.x-1).isFull)
          true
        else if(d.y == b.row.size && !b.row(d.y-1).square(d.x-1).isFull)
          true
        else if(d.y < b.row.size && d.y > 0 && !b.row(d.y).square(d.x-1).isFull
          && !b.row(d.y-1).square(d.x-1).isFull)
          true
        else
          false
      }
      else
        false
    }
    else
      false
  }

  def makeConnection(dot:Dot, toDot:Dot) = {
    dot.links.add(toDot)
    toDot.links.add(dot)
    if(toDot.y - dot.y < 0){
      if(dot.x > b.row(0).square.size-1)
        boards.setConnector(i, toDot.y, toDot.x-1, "Right", s = true, l = false)
      else
        boards.setConnector(i, toDot.y, toDot.x, "Left", s = true, l = false)
    }
    else if(toDot.x - dot.x > 0){
      if(dot.y > b.row.size-1)
        boards.setConnector(i, dot.y-1, dot.x, "Down", s = true, l = false)
      else
        boards.setConnector(i, dot.y, dot.x, "Up", s = true, l = false)
    }
    else if(toDot.y - dot.y > 0){
      if(dot.x > b.row(dot.y).square.size-1)
        boards.setConnector(i, dot.y, dot.x-1, "Right", s = true, l = false)
      else
        boards.setConnector(i, dot.y, dot.x, "Left", s = true, l = false)
    }
    else{
      if (dot.y > b.row.size-1)
        boards.setConnector(i, toDot.y-1, toDot.x, "Down", s = true, l = false)
      else
        boards.setConnector(i, toDot.y, toDot.x, "Up", s = true, l = false)
    }
  }

  def removeConnection(dot:Dot, prevDot:Dot) = {
    dot.links.remove(prevDot)
    prevDot.links.remove(dot)
    if(prevDot.y - dot.y > 0){
      if(dot.x > b.row(dot.y).square.size-1)
        boards.setConnector(i, dot.y, dot.x-1, "Right", s = false, l = false)
      else
        boards.setConnector(i, dot.y, dot.x, "Left", s = false, l = false)
    }
    else if(prevDot.x - dot.x < 0){
      if(prevDot.y > b.row.size-1)
        boards.setConnector(i, prevDot.y-1, prevDot.x, "Down", s = false, l = false)
      else
        boards.setConnector(i, prevDot.y, prevDot.x, "Up", s = false, l = false)
    }
    else if(prevDot.y - dot.y < 0){
      if(prevDot.x > b.row(prevDot.y).square.size-1)
        boards.setConnector(i, prevDot.y, prevDot.x-1, "Right", s = false, l = false)
      else
        boards.setConnector(i, prevDot.y, prevDot.x, "Left", s = false, l = false)
    }
    else if(prevDot.x - dot.x > 0){
      if (dot.y > b.row.size-1)
        boards.setConnector(i, dot.y-1, dot.x, "Down", s = false, l = false)
      else
        boards.setConnector(i, dot.y, dot.x, "Up", s = false, l = false)
    }
  }

  //TODO ensure complete loop
  def solved:Boolean = {
    for (y <- b.row){
      for(x <- y.square)
        if (x.value != -1 && !x.isFull)
          return false
    }
    true
  }

}

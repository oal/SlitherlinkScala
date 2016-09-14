class Solver(val board: Board) {
  Rules.applyRules(board)
  Rules.applyThrees(board)

  val dots = new Dots                                                             //Holds all the dots on the board
  initDots()
                                                                    //Call the function start to begin solving the board
  /**
    * @version 1.0 Sep 13, 2016.
    * Dots has a container 'x' whose indices are x-coordinates and contents is of class x
    */
  class Dots{
    val x = for ( x <- 0 to board.row(0).square.size) yield new x(x)                  //x is a container holding all ys for that x
  }

  /**
    * @version 1.0 Sep 13, 2016.
    * x has a container 'y' whose indices are y-coordinates and contents is of class Dot.
    * creates a Dot for each y for the given x parameter
    * @param x
    */
  class x(x:Int){
    val y = for ( y <- 0 to board.row.size) yield new Dot(x, y)                       //y is a container holding all dots for that y in the param x
  }

  /**
    * @version 1.0 Sep 13, 2016.
    * A dot with the given x and y positions given in the parameter.
    * Dot has an object 'links' of class Links whose contents are the neighboring dots with which it is connected
    * @param xPos
    * @param yPos
    */
  class Dot(xPos:Int, yPos:Int){
    val x = xPos
    val y = yPos
    val links = new Links(null, null)
  }

  /**
    * @version 1.0 Sep 13, 2016.
    * Holds two dots with which another dot shares a connection
    * @param dot1
    * @param dot2
    */
  class Links(dot1:Dot, dot2:Dot){
    var d1 = dot1
    var d2 = dot2

    def getNonEmpty:Dot = if(d1 != null) d1 else if(d2 != null) d2 else null
    def empty:Boolean = d1 == null && d2 == null
    def full:Boolean = d1 != null && d2 != null                                   //returns boolean value true if both dots are not null
    def add(d:Dot) = if(d1 == null && !contains(d)) d1 = d
                      else if(d2 == null && !contains(d)) d2 = d                  //add parameter dot to whichever dot is null
    def contains(d:Dot) = d1 == d || d2 == d                                      //return boolean value true if the parameter dot is either of the dots
    def remove(d:Dot) = if(d1 == d) d1 = null else if(d2 == d) d2 = null          //sets a dot to null if the parameter dot is the same as that dot
  }

  /**
    * @version 1.1 Sep 14, 2016.
    * calls move with starting point at the parameter coordinates
    */
  def solve() = {
    if(findStartingDot._1 != null)
      move(findStartingDot._1, findStartingDot._2)
  }

  /**
    * @version 1.1 Sep 14, 2016.
    * From the given parameter dot, check if a move in the directions 'Up, 'Right, 'Down & 'Left respectively
    * are valid moves, if so calls itself recursively. If no valid moves can be made, check to see if the board is solved.
    * If the board is solved no other moves are valid and backtracks without making any further calls to itself. If however
    * the board is not solved and none of the moves are valid, removes the connection between the dot and the previous dot
    * and backtracks 1 step to the call with the previous dot and proceeds. When completed, given a valid starting point,
    * produces a linked list as the solution.
    * @param dot
    * @param previousDot
    */
  def move(dot:Dot, previousDot:Dot):Unit = {
    if(validMove(dot, 'Up)){                                                     //checks if up is a valid move
      makeConnection(dot, dots.x(dot.x).y(dot.y-1))                               //makes the connections between the dot and the dot above
      move(dots.x(dot.x).y(dot.y-1), dot)                                         //calls move with the above dot
    }
    if(validMove(dot, 'Right)){                                                  //checks if right is a valid move
      makeConnection(dot, dots.x(dot.x+1).y(dot.y))                               //makes the connections between the dot and the right dot
      move(dots.x(dot.x+1).y(dot.y), dot)                                         //calls move with the right dot
    }
    if(validMove(dot, 'Down)){                                                   //checks if down is a valid move
      makeConnection(dot, dots.x(dot.x).y(dot.y+1))                               //makes the connections between the dot and the dot below
      move(dots.x(dot.x).y(dot.y+1), dot)                                         //calls move with the dot below
    }
    if(validMove(dot, 'Left)){                                                   //checks if left is a valid move
      makeConnection(dot, dots.x(dot.x-1).y(dot.y))                               //makes the connection between the dot and the left dot
      move(dots.x(dot.x-1).y(dot.y), dot)                                         //calls move with the left dot
    }
    if(dot.links.full && !loop(dot)){                                             //calls start to find new point to move from if dot is full but not in a loop
      solve()
    }
    if(!solved){                                                                  //checks if the board is solved
      removeConnection(dot, previousDot)                                          //if not remove the connection between the dot and previous dot
    }
  }

  def loop(d:Dot):Boolean = {
    def next(n:Dot, p:Dot):Boolean = {
      if(n.links.full && n==d)
        true
      else if(n.links.full){
        if(n.links.d1 == p)
          next(n.links.d2, n)
        else
          next(n.links.d1, n)
      }
      else
        false
    }

    next(d.links.d1, d)
  }

  /**
    * @version 1.0 Sep 13, 2016.
    * Takes in a param Dot and a direction and checks to see if a move in that direction is permitted
    * @param d:Dot
    * @param s:String (Direction)
    * @return Boolean
    */
  def validMove(d:Dot, s:Symbol):Boolean = {
    if(d.links.full)                                                              //If all links are saturated no moves are valid
      false
    else if(s == 'Up){
      if(d.y > 0                                                                  //If the dot's y is 0 or the dot above is full or contains this dot
        && !dots.x(d.x).y(d.y-1).links.full                                       //no moves in the upward direction is allowed
        && !dots.x(d.x).y(d.y-1).links.contains(d))
      {
        if(d.x == 0                                                               //If the dot's x is 0 check if the northeast square is full
          && !board.row(d.y-1).square(d.x).isFull)                                    //if not the move is allowed
          true
        else if(d.x == board.row(0).square.size                                       //If the dot's x is at the right edge check if the northwest square is full
          && !board.row(d.y-1).square(d.x-1).isFull)                                  //if not the move is allowed
          true
        else if (d.x > 0                                                          //If the dot is at neither of x's edges, check if both north squares are full
          && d.x < board.row(0).square.size                                           //if neither are full the move is allowed
          && !board.row(d.y-1).square(d.x-1).isFull
          && !board.row(d.y-1).square(d.x).isFull)
          true
        else
          false
      }
      else
        false
    }
    else if(s == 'Right){
      if(d.x < dots.x.size-1                                                      //If the dot's x is at the bottom edge or the dot below is full or contains this dot
        && !dots.x(d.x+1).y(d.y).links.full                                       //no moves in the downward direction is allowed
        && !dots.x(d.x+1).y(d.y).links.contains(d))
      {
        if (d.y == 0                                                              //If the dot's y is 0 check if the southeast square is full
          && !board.row(d.y).square(d.x).isFull)                                      //if not the move is allowed
          true
        else if(d.y == board.row.size                                                 //If the dot's y is at the bottom edge check if the northeast square is full
          && !board.row(d.y-1).square(d.x).isFull)                                    //if not the move is allowed
          true
        else if(board.row.size > d.y                                                  //If the dot is at neither of y's edges, check if both east squares are full
          && d.y > 0                                                              //if neither are full the move is allowed
          && !board.row(d.y).square(d.x).isFull
          && !board.row(d.y-1).square(d.x).isFull)
          true
        else
          false
      }
      else
        false
    }
    else if(s == 'Down){
      if(d.y < dots.x(d.x).y.size-1                                               //If the dot's y is at the bottom edge or the dot above is full or contains this dot
        && !dots.x(d.x).y(d.y+1).links.full                                       //no moves in the upward direction is allowed
        && !dots.x(d.x).y(d.y+1).links.contains(d))
      {
        if(d.x == 0                                                               //If the dot's x is 0 check if the southeast square is full
          && !board.row(d.y).square(d.x).isFull)                                      //if not the move is allowed
          true
        else if(d.x == board.row(0).square.size                                       //If the dot's x is at the right edge check if the southwest square is full
          && !board.row(d.y).square(d.x-1).isFull)                                    //if not the move is allowed
          true
        else if(d.x > 0                                                           //If the dot is at neither of x's edges, check if both south squares are full
          && d.x < board.row(d.y).square.size                                         //if neither are full the move is allowed
          && !board.row(d.y).square(d.x).isFull
          && !board.row(d.y).square(d.x-1).isFull)
          true
        else
          false
      }
      else
        false
    }
    else if(s == 'Left){
      if(d.x > 0                                                                  //If the dot's x is 0 or the dot below is full or contains this dot
        && !dots.x(d.x-1).y(d.y).links.full                                       //no moves in the downward direction is allowed
        && !dots.x(d.x-1).y(d.y).links.contains(d))
      {
        if(d.y == 0                                                               //If the dot's y is 0 check if the southwest square is full
          && !board.row(d.y).square(d.x-1).isFull)                                    //if not the move is allowed
          true
        else if(d.y == board.row.size                                                 //If the dot's y is at the bottom edge check if the northwest square is full
          && !board.row(d.y-1).square(d.x-1).isFull)                                  //if not the move is allowed
          true
        else if(d.y < board.row.size                                                  //If the dot is at neither of y's edges, check if both west squares are full
          && d.y > 0                                                              //if neither are full the move is allowed
          && !board.row(d.y).square(d.x-1).isFull
          && !board.row(d.y-1).square(d.x-1).isFull)
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

  def makeConnection(dot:Dot, toDot:Dot):Unit = {
    makeConnection(dot, toDot, l = false)
  }

  /**
    * @version 1.0 Sep 13, 2016.
    * Sets the connection between dot and toDot by adding each other to their links object
    * and setting the connector in the respective square connector direction.
    * @param dot
    * @param toDot
    * @return Unit
    */
  def makeConnection(dot:Dot, toDot:Dot, l:Boolean) = {
    dot.links.add(toDot)
    toDot.links.add(dot)
    if(toDot.y - dot.y < 0){
      if(dot.x > board.row(0).square.size-1)
        board.setConnector(toDot.y, toDot.x-1, 'Right, s = true, l)
      else
        board.setConnector(toDot.y, toDot.x, 'Left, s = true, l)
    }
    else if(toDot.x - dot.x > 0){
      if(dot.y > board.row.size-1)
        board.setConnector(dot.y-1, dot.x, 'Down, s = true, l)
      else
        board.setConnector(dot.y, dot.x, 'Up, s = true, l)
    }
    else if(toDot.y - dot.y > 0){
      if(dot.x > board.row(dot.y).square.size-1)
        board.setConnector(dot.y, dot.x-1, 'Right, s = true, l)
      else
        board.setConnector(dot.y, dot.x, 'Left, s = true, l)
    }
    else{
      if (dot.y > board.row.size-1)
        board.setConnector(toDot.y-1, toDot.x, 'Down, s = true, l)
      else
        board.setConnector(toDot.y, toDot.x, 'Up, s = true, l)
    }
  }

  def removeConnection(dot:Dot, prevDot:Dot) = {
    if(prevDot.y - dot.y > 0){
      if(dot.x > board.row(dot.y).square.size-1
        && !board.row(dot.y).square(dot.x-1).connector('Right).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(dot.y, dot.x-1, 'Right, s = false, l = false)
      }
      else if(dot.x <= board.row(dot.y).square.size-1
        && !board.row(dot.y).square(dot.x).connector('Left).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(dot.y, dot.x, 'Left, s = false, l = false)
      }
    }
    else if(prevDot.x - dot.x < 0){
      if(prevDot.y > board.row.size-1
        && !board.row(prevDot.y-1).square(prevDot.x).connector('Down).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(prevDot.y-1, prevDot.x, 'Down, s = false, l = false)
      }
      else if(prevDot.y <= board.row.size-1
        && !board.row(prevDot.y).square(prevDot.x).connector('Up).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(prevDot.y, prevDot.x, 'Up, s = false, l = false)
      }
    }
    else if(prevDot.y - dot.y < 0){
      if(prevDot.x > board.row(prevDot.y).square.size-1
        && !board.row(prevDot.y).square(prevDot.x-1).connector('Right).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(prevDot.y, prevDot.x-1, 'Right, s = false, l = false)
      }
      else if(prevDot.x <= board.row(prevDot.y).square.size-1
        && !board.row(prevDot.y).square(prevDot.x).connector('Left).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(prevDot.y, prevDot.x, 'Left, s = false, l = false)
      }
    }
    else if(prevDot.x - dot.x > 0){
      if(dot.y > board.row.size-1
        && !board.row(dot.y-1).square(dot.x).connector('Down).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(dot.y-1, dot.x, 'Down, s = false, l = false)
      }
      else if(dot.y <= board.row.size-1
        && !board.row(dot.y).square(dot.x).connector('Up).locked)
      {
        dot.links.remove(prevDot)
        prevDot.links.remove(dot)
        board.setConnector(dot.y, dot.x, 'Up, s = false, l = false)
      }
    }
  }

  def solved(d:Dot):Boolean = {
    for(y <- board.row;
        x <- y.square)
      if (x.value != -1 && !x.isFull)
        return false
    if(!loop(d))
      false
    else
      true
  }

  def findStartingDot:(Dot, Dot) = {
    for(x <- dots.x;
      y <- x.y)
      if(!y.links.empty && !y.links.full)
        return (y, y.links.getNonEmpty)
    (null, null)
  }

  def initDots(): Unit = {
    for(y <- board.row;
      x <- y.square;
      c <- x.connector)
      if(c._2.set){
        if(c._1 == 'Up)
          makeConnection(dots.x(x.x).y(x.y), dots.x(x.x+1).y(x.y), c._2.locked)
        else if(c._1 == 'Right)
          makeConnection(dots.x(x.x+1).y(x.y), dots.x(x.x+1).y(x.y+1), c._2.locked)
        else if(c._1 == 'Down)
          makeConnection(dots.x(x.x).y(x.y+1), dots.x(x.x+1).y(x.y+1), c._2.locked)
        else if(c._1 == 'Left)
          makeConnection(dots.x(x.x).y(x.y), dots.x(x.x).y(x.y+1), c._2.locked)
      }
  }

}

/**
  * Created by Torsvik on 06/09/2016.
  */

class Solver(boards:Boards, i:Int) {

  val getBoard = new Corners(new Threes(boards, i).getBoard, i).getBoard

}

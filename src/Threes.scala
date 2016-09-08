/**
  * Created by Torsvik on 06/09/2016.
  */

class Threes(boards:Boards, i:Int) {
  private def b = boards.board(i)

  for( y <- b.row){
    for( x <- y.square ){
      if( x.value == 3 ){

        //Tries to find any value zero adjacent to 3 and sets connectors according to direction
        if( x.y > 0 && b.row(x.y-1).square(x.x).value == 0 ){                     //Tries to find a zero above the square
          boards.setConnector(i, x.y, x.x, "Left", s = true, l = true)                      //Sets the left connector of the square
          boards.setConnector(i, x.y, x.x, "Right", s = true, l = true)                     //Sets the right connector of the square
          boards.setConnector(i, x.y, x.x, "Down", s = true, l = true)                      //Sets the bottom connector of the square
          boards.setConnector(i, x.y-1, x.x+1, "Down", s = true, l = true)                  //Sets the bottom connector of the square in the "northeast" diagonal of the original
          boards.setConnector(i, x.y-1, x.x-1, "Down", s = true, l = true)                  //Sets the bottom connector of the square in the "northwest" diagonal of the original
        }
        else if( x.y < b.row.size-1 && b.row(x.y+1).square(x.x).value == 0 ){     //Tries to find a zero below the square
          boards.setConnector(i, x.y, x.x, "Left", s = true, l = true)                      //Sets the left connector of the square
          boards.setConnector(i, x.y, x.x, "Right", s = true, l = true)                     //Sets the right connector of the square
          boards.setConnector(i, x.y, x.x, "Up", s = true, l = true)                        //Sets the top connector of the square
          boards.setConnector(i, x.y+1, x.x+1, "Up", s = true, l = true)                    //Sets the top connector of the square in the "southeast" diagonal of the original
          boards.setConnector(i, x.y+1, x.x-1, "Up", s = true, l = true)                    //Sets the top connector of the square in the "southwest" diagonal of the original
        }
        else if( x.x > 0 && b.row(x.y).square(x.x-1).value == 0 ){                //Tries to find a zero left of the square
          boards.setConnector(i, x.y, x.x, "Right", s = true, l = true)                     //Sets the right connector of the square
          boards.setConnector(i, x.y, x.x, "Down", s = true, l = true)                      //Sets the bottom connector of the square
          boards.setConnector(i, x.y, x.x, "Up", s = true, l = true)                        //Sets the top connector of the square
          boards.setConnector(i, x.y-1, x.x-1, "Right", s = true, l = true)                 //Sets the right connector of the square in the "northwest" diagonal of the original
          boards.setConnector(i, x.y+1, x.x-1, "Right", s = true, l = true)                 //Sets the right connector of the square in the "southwest" diagonal of the original
        }
        else if( x.x < b.row(x.y).square.size - 1
                && b.row(x.y).square(x.x+1).value == 0 ){                         //Tries to find a zero right of the square
          boards.setConnector(i, x.y, x.x, "Left", s = true, l = true)                      //Sets the left connector of the square
          boards.setConnector(i, x.y, x.x, "Down", s = true, l = true)                      //Sets the bottom connector of the square
          boards.setConnector(i, x.y, x.x, "Up", s = true, l = true)                        //Sets the top connector of the square
          boards.setConnector(i, x.y-1, x.x+1, "Left", s = true, l = true)                  //Sets the right connector of the square in the "northeast" diagonal of the original
          boards.setConnector(i, x.y+1, x.x+1, "Left", s = true, l = true)                  //Sets the right connector of the square in the "southeast" diagonal of the original
        }

        if( x.y > 0 && x.x > 0 && b.row(x.y-1).square(x.x-1).value == 0 ){        //Tries to find a zero "northwest" of the square
          boards.setConnector(i, x.y, x.x, "Up", s = true, l = true)                        //Sets the top connector of the square
          boards.setConnector(i, x.y, x.x, "Left", s = true, l = true)                      //Sets the left connector of the square
        }

        if( x.y > 0 && x.x < b.row(x.y).square.size - 1
            && b.row(x.y-1).square(x.x+1).value == 0 ){                           //Tries to find a zero "northeast" of the square
          boards.setConnector(i, x.y, x.x, "Up", s = true, l = true)                        //Sets the top connector of the square
          boards.setConnector(i, x.y, x.x, "Right", s = true, l = true)                     //Sets the right connector of the square
        }

        if( x.y < b.row.size-1 && x.x > 0
            && b.row(x.y+1).square(x.x-1).value == 0 ){                           //Tries to find a zero "southwest" of the square
          boards.setConnector(i, x.y, x.x, "Down", s = true, l = true)                      //Sets the bottom connector of the square
          boards.setConnector(i, x.y, x.x, "Left", s = true, l = true)                      //Sets the left connector of the square
        }

        if( x.y < b.row.size-1 && x.x < b.row(x.y).square.size - 1
          && b.row(x.y+1).square(x.x+1).value == 0 ){                             //Tries to find a zero "southeast" of the square
          boards.setConnector(i, x.y, x.x, "Down", s = true, l = true)                      //Sets the bottom connector of the square
          boards.setConnector(i, x.y, x.x, "Right", s = true, l = true)                     //Sets the right connector of the square
        }

        //Tries to find any value zero adjacent to 3 and sets connectors according to direction
        if(  x.y > 0 && b.row(x.y-1).square(x.x).value == 3 ){                    //Tries to find a three above the square
          boards.setConnector(i, x.y, x.x, "Up", s = true, l = true)                        //Sets the top connector of the square
          boards.setConnector(i, x.y, x.x, "Down", s = true, l = true)                      //Sets the bottom connector of the square
          boards.setConnector(i, x.y-1, x.x, "Up", s = true, l = true)                      //Sets the bottom connector of the adjacent square
        }

        if( x.y < b.row.size-1 && b.row(x.y+1).square(x.x).value == 3 ){          //Tries to find a three below the square
          boards.setConnector(i, x.y, x.x, "Up", s = true, l = true)                        //Sets the top connector of the square
          boards.setConnector(i, x.y, x.x, "Down", s = true, l = true)                      //Sets the bottom connector of the square
          boards.setConnector(i, x.y+1, x.x, "Down", s = true, l = true)                    //Sets the bottom connector of the adjacent square
        }

        if( x.x > 0 && b.row(x.y).square(x.x-1).value == 3 ){                     //Tries to find a three left of the square
          boards.setConnector(i, x.y, x.x, "Right", s = true, l = true)                     //Sets the right connector of the square
          boards.setConnector(i, x.y, x.x, "Left", s = true, l = true)                      //Sets the left connector of the square
          boards.setConnector(i, x.y, x.x-1, "Left", s = true, l = true)                    //Sets the left connector of the adjacent square
        }

        if( x.x < b.row(x.y).square.size - 1
          && b.row(x.y).square(x.x+1).value == 0 ){                               //Tries to find a three right of the square
          boards.setConnector(i, x.y, x.x, "Right", s = true, l = true)                     //Sets the right connector of the square
          boards.setConnector(i, x.y, x.x, "Left", s = true, l = true)                      //Sets the left connector of the square
          boards.setConnector(i, x.y, x.x+1, "Right", s = true, l = true)                   //Sets the right connector of the adjacent square
        }

      }
    }
  }

  def getBoard = boards
}

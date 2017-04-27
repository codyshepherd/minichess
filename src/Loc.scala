/**
  * Created by cody on 4/26/17.
  */

/** Loc is just a named pair of x, y coordinates, where x is the row,
  * and y is the column of the board.
  * */
sealed class Loc(val x: Int, val y: Int) {

  override def equals(o: Any): Boolean = {
    o match {
      case that: Loc => {
        if (this.x == that.x && this.y == that.y)
          true
        else
          false
      }
      case that: (Int, Int) => if(this.x == that._1 && this.y == that._2) true else false
      case _ => false
    }
  }
}

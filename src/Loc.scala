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

  def toColRow: (Col, Row) = {
    val row: Row = this.x match {
      case 0 => R1()
      case 1 => R2()
      case 2 => R3()
      case 3 => R4()
      case 4 => R5()
      case 5 => R6()
      case _ => Z()
    }
    val col: Col = this.y match {
      case 0 => A()
      case 1 => B()
      case 2 => C()
      case 3 => D()
      case 4 => E()
      case _ => X()
    }

    (col, row)
  }
}

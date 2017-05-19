/** Loc.scala
  * minichess
  * Cody Shepherd
  */

/** Loc is just a named pair of x, y coordinates, where x is the row,
  * and y is the column of the board.
  *
  * It should be noted that Loc envisions the _bottom_ of the board as being
  * row 0. In other words, the white King sits at 0,4, while the black King
  * sits at 5,0 at the start of the game.
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
      case (a: Int, b: Int) => if(this.x == a && this.y == b) true else false
      case _ => false
    }
  }

  override def toString: String = {
    x.toString + " " + y.toString
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

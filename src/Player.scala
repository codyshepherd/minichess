/** Player.scala
  * minichess
  * Cody Shepherd
  */

/** The Player ADT represents a marker for Black or White; whomever is on move.
  *
  * op() is a method that allows me to derive movement locations in a
  * generic way.
  *
  * opposite() allows me to derive the next player easily.
  * */
sealed abstract class Player {
  def op(x: Int): Int
  def opposite: Player
}
case class Black() extends Player {
  def op(x: Int): Int = -x
  def opposite: Player = White()
}
case class White() extends Player {
  def op(x: Int): Int = x
  def opposite: Player = Black()
}

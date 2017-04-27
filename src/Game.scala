/**
  * Created by cody on 4/26/17.
  */

/** This is the game Referee; i.e. it pits two Agents against one another.
  *
  * Game "owns" the board; it takes a move from each player on its turn and updates the
  * board accordingly.
  *
  * Keeps track of time
  * */
class Game {

}

object Params {
  val cols = 5
  val rows = 6

  val top = 5
  val bottom = 0

  val pawn = 1.0
  val knight = 3.0
  val bishop = 3.0
  val rook = 5.0
  val queen = 9.0

  private[this] var _w_time: Double = 5.0

  def w_time: Double = _w_time

  def w_time_=(value: Double): Unit = {
    _w_time = value
  }

  private[this] var _b_time: Double = 5.0

  def b_time: Double = _b_time

  def b_time_=(value: Double): Unit = {
    _b_time = value
  }

  //TODO: Global time

  def computeVals(l: List[Piece]): (Double, Double) = {
    var white = 0.0
    var black = 0.0

    for(p <- l){
      p.getPlayer match {
        case Black() => black += p.value
        case White() => white += p.value
      }
    }
    (black, white)
  }
}
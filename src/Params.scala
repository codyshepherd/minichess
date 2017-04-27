/**
  * Created by cody on 4/27/17.
  */
import java.time.LocalTime
import java.time.ZoneId

/** Static global values
  * */
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

  val startTime: Int = LocalTime.now(ZoneId.systemDefault()).toSecondOfDay

  private[this] var _w_time: Int = 300

  def w_time: Int = _w_time

  def w_time_=(value: Int): Unit = {
    _w_time = value
  }

  private[this] var _b_time: Int = 300

  def b_time: Int = _b_time

  def b_time_=(value: Int): Unit = {
    _b_time = value
  }

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

  def bOutOfTime : Boolean = {
    if(b_time <= 0)
      true
    else
      false
  }

  def wOutOfTime : Boolean = {
    if(w_time <= 0)
      true
    else
      false
  }

  def stringsToState(s: List[String]): State = {
    var i = Params.top
    var j = 0

    val p = s.head

    var ps = List[Piece]()
    for (row <- s.tail){
      for(item <- row){
        item match {
          case 'k' => ps :+ King(Black(), new Loc(i, j))
          case 'K' => ps :+ King(White(), new Loc(i, j))
          case 'q' => ps :+ Queen(Black(), new Loc(i, j))
          case 'Q' => ps :+ Queen(White(), new Loc(i, j))
          case 'b' => ps :+ Bishop(Black(), new Loc(i, j))
          case 'B' => ps :+ Bishop(White(), new Loc(i, j))
          case 'n' => ps :+ Knight(Black(), new Loc(i, j))
          case 'N' => ps :+ Knight(White(), new Loc(i, j))
          case 'r' => ps :+ Rook(Black(), new Loc(i, j))
          case 'R' => ps :+ Rook(White(), new Loc(i, j))
          case 'p' => ps :+ Pawn(Black(), new Loc(i, j))
          case 'P' => ps :+ Pawn(White(), new Loc(i, j))
        }
        j += 1
      }
      i -= 1
    }

    val vals = computeVals(ps)

    new State(on_move = if(p.toLowerCase.contains("w")) White() else Black(),
      moveNum = p.head.toInt,
      b_value = vals._1,
      w_value = vals._2,
      pieces = ps)
  }


}

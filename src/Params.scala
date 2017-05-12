/**
  * Created by cody on 4/27/17.
  */
import java.time.LocalTime
import java.time.ZoneId
import scala.io.Source

/** Static global values
  * */
object Params {
  var plyDepth: Int = 6
  var turnTime: Int = 7

  val cols = 5
  val rows = 6

  val top = 5
  val bottom = 0

  val leftLimit = 0
  val rightLimit = 4

  val pawn = 1.0
  val knight = 3.0
  val bishop = 3.0
  val rook = 5.0
  val queen = 9.0
  val king = 50.0

  val blackValue:Long = scala.util.Random.nextLong()
  val whiteValue:Long = scala.util.Random.nextLong()

  val startTime: Int = LocalTime.now(ZoneId.systemDefault()).toSecondOfDay

  var ttable: Map[Long, Tpos] = Map()

  val ztable: Array[Array[Long]] = for(x <- Array.range(0,rows*cols)) yield for(y <- Array.range(0,12)) yield scala.util.Random.nextLong()

  val pieceIndices: Map[String, Int] = Map(
    "P" -> 0,
    "N" -> 1,
    "B" -> 2,
    "R" -> 3,
    "Q" -> 4,
    "K" -> 5,
    "p" -> 6,
    "n" -> 7,
    "b" -> 8,
    "r" -> 9,
    "q" -> 10,
    "k" -> 11
  )

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
      j = 0
      for(item <- row){
        item match {
          case 'k' => ps = ps :+ King(Black(), new Loc(i, j))
          case 'K' => ps = ps :+ King(White(), new Loc(i, j))
          case 'q' => ps = ps :+ Queen(Black(), new Loc(i, j))
          case 'Q' => ps = ps :+ Queen(White(), new Loc(i, j))
          case 'b' => ps = ps :+ Bishop(Black(), new Loc(i, j))
          case 'B' => ps = ps :+ Bishop(White(), new Loc(i, j))
          case 'n' => ps = ps :+ Knight(Black(), new Loc(i, j))
          case 'N' => ps = ps :+ Knight(White(), new Loc(i, j))
          case 'r' => ps = ps :+ Rook(Black(), new Loc(i, j))
          case 'R' => ps = ps :+ Rook(White(), new Loc(i, j))
          case 'p' => ps = ps :+ Pawn(Black(), new Loc(i, j))
          case 'P' => ps = ps :+ Pawn(White(), new Loc(i, j))
          case _ =>
        }
        j += 1
      }
      i -= 1
    }

    val vals = computeVals(ps)

    val state: State = new State(on_move = if(p.toLowerCase.contains("w")) White() else Black(),
      moveNum = p.takeWhile((p: Char) => p != ' ').toInt,
      b_value = vals._1,
      w_value = vals._2,
      pieces = ps)

    //System.err.println("stringToState state s: \n" +  state.toString)

    state
  }

  def stateFromFile(file: String): State = {
    val lines: Iterator[String] = Source.fromFile(file).getLines()

    Params.stringsToState(lines.toList)
  }

  def zobristHash(s: State, depth: Int): Long = {
    var h:Long = 0
    for(i <- List.range(0,rows)){
      for(j <- List.range(0,cols)){
        //System.err.println("i: " + i + " j: " + j)
        val pc = s.pieces.find((p: Piece) => p.getLoc == (i,j))
        if(pc.isDefined){
          val ind = pieceIndices get pc.get.toString
          //System.err.println("ind: " + ind.get)
          if(ind.isDefined)
            h = h ^ ztable(i*cols + j)(ind.get)
        }
      }
    }
    if(s.on_move == White())
      h = h ^ whiteValue ^ depth
    else
      h = h ^ blackValue ^ depth
    h
  }
}

/**
  * Created by cody on 4/27/17.
  */
import java.time.LocalTime
import java.time.ZoneId
import scala.io.Source

/** Static global values
  * */
object Params {
  var path: String = "C:/Users/codys/IdeaProjects/minichess/src/"

  var mobility: Boolean = false
  var isTtableOn: Boolean = true
  var think: Boolean = false

  var plyDepth: Int = 6
  var turnTime: Int = 7
  var startTime: Int = LocalTime.now(ZoneId.systemDefault()).toSecondOfDay
  var cachedBestMove: Move = new Noop()
  var cachedBestMoveVal: Double = 0.0

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

  val mvWeight = 0.8
  val mbWeight = 0.2

  assert(mvWeight + mbWeight == 1.0)

  val blackValue:Long = scala.util.Random.nextLong()
  val whiteValue:Long = scala.util.Random.nextLong()

  var ttable: scala.collection.mutable.Map[Long, Tpos] = scala.collection.mutable.Map()

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
    "k" -> 11,
    "0" -> 12,
    "1" -> 13,
    "2" -> 14,
    "3" -> 15,
    "4" -> 16,
    "5" -> 17,
    "6" -> 18,
    "7" -> 19,
    "8" -> 20,
    "9" -> 21
  )

  val ztable: Array[Array[Long]] = for(x <- Array.range(0,(rows+1)*cols)) yield for(y <- Array.range(0,pieceIndices.keys.size)) yield scala.util.Random.nextLong()
  //val ztable: Array[Long] = for(x <- Array.range(0,(rows*2)*cols)) yield scala.util.Random.nextLong()

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

  def stringsToState(s: List[String]): State = {
    var i = Params.top
    var j = 0

    val p = s.head

    var ps = List[Piece]()
    for (row <- s.tail){
      j = 0
      for(item <- row){
        item match {
          case 'k' => ps = King(Black(), new Loc(i, j)) :: ps
          case 'K' => ps = King(White(), new Loc(i, j)) :: ps
          case 'q' => ps = Queen(Black(), new Loc(i, j)) :: ps
          case 'Q' => ps = Queen(White(), new Loc(i, j)) :: ps
          case 'b' => ps = Bishop(Black(), new Loc(i, j)) :: ps
          case 'B' => ps = Bishop(White(), new Loc(i, j)) :: ps
          case 'n' => ps = Knight(Black(), new Loc(i, j)) :: ps
          case 'N' => ps = Knight(White(), new Loc(i, j)) :: ps
          case 'r' => ps = Rook(Black(), new Loc(i, j)) :: ps
          case 'R' => ps = Rook(White(), new Loc(i, j)) :: ps
          case 'p' => ps = Pawn(Black(), new Loc(i, j)) :: ps
          case 'P' => ps = Pawn(White(), new Loc(i, j)) :: ps
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

  def getLegalMoves(s: State): List[Move] = {
    //System.err.println("getLegalMoves s argument: " + s.toString)

    val mypieces = s.pieces.filter((p:Piece) => p.getPlayer == s.on_move)

    //System.err.println("getLegalMoves mypieces: " + mypieces.toString())

    var moves: List[Move] = List()

    for (piece <- mypieces){
      val pieceMoves = piece.legalMoves(s)
      for(move <- pieceMoves){
        moves = stringToMove(piece, move) :: moves
      }
    }
    moves.distinct
  }

  def stringToMove(p: Piece, s: String): Move = {
    val newLoc = p.getMovLoc(s)
    val fromRow: Row = p.getLoc.x match {
      case 0 => R1()
      case 1 => R2()
      case 2 => R3()
      case 3 => R4()
      case 4 => R5()
      case 5 => R6()
      case _ => Z()
    }
    val fromCol: Col = p.getLoc.y match {
      case 0 => A()
      case 1 => B()
      case 2 => C()
      case 3 => D()
      case 4 => E()
      case _ => X()
    }
    val toRow: Row = newLoc.x match {
      case 0 => R1()
      case 1 => R2()
      case 2 => R3()
      case 3 => R4()
      case 4 => R5()
      case 5 => R6()
      case _ => Z()
    }
    val toCol: Col = newLoc.y match {
      case 0 => A()
      case 1 => B()
      case 2 => C()
      case 3 => D()
      case 4 => E()
      case _ => X()
    }
    new Move(p, s, (fromRow, fromCol), (toRow, toCol))
  }

  /*
  def zobristHash(s: State, depth: Int): Long = {
    var h: Long = 0
    var i = -1
    for(c <- s.toString) {
      i += 1
      h = h ^ c.toLong ^ ztable(i)
    }
    System.err.println("Hash key: " + h)
    h
  }
  */

  def zobristHash(s: State, depth: Int): Long = {
    var h:Long = 0
    val sorted = s.pieces.sortBy(p => (p.getLoc.x, p.getLoc.y))

    /*
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
    */
    for(piece <- sorted) {
      val ind = pieceIndices get piece.toString
      if(ind.isDefined)
        h = h ^ ztable(piece.getLoc.x*cols + piece.getLoc.y)(ind.get)
    }

    if(s.on_move == White())
      //h = h ^ whiteValue ^ depth
      h = h ^ whiteValue ^ s.moveNum //^ depth
    else
      //h = h ^ blackValue ^ depth
      h = h ^ blackValue ^ s.moveNum //^ depth
    h
  }
}

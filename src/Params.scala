/** Params.scala
  * minichess
  * Cody Shepherd
  */

import java.time.LocalTime
import java.time.ZoneId

import scala.collection.mutable.ListBuffer
import scala.io.Source

/** This object holds global configuration values, plus a handful of "helper" functions
  * that potentially need to be accessed by several classes but shouldn't require
  * unique instances of a class.
  * */
object Params {
  var path: String = "C:/Users/codys/IdeaProjects/minichess/src/"

  /** Runtime option flags.
    * */
  var mobility: Boolean = false
  var isTtableOn: Boolean = true
  var think: Boolean = false
  var plyDepth: Int = 21
  var turnTime: Int = 5

  /** Globally - cached best moves and values per turn.
    * */
  var startTime: Int = LocalTime.now(ZoneId.systemDefault()).toSecondOfDay
  var cachedBestMove: Move = new Noop()
  var cachedBestMoveVal: Double = 0.0

  /** Global constants
    * */
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

  /** Startup-time random values for use in Zobrist hash.
    * */
  val blackValue:Long = scala.util.Random.nextLong()
  val whiteValue:Long = scala.util.Random.nextLong()

  /** Transposition table
    * */
  var ttable: scala.collection.mutable.Map[Long, Tpos] = scala.collection.mutable.Map()

  /** Mapping to facilitate zobrist hashing.
    * */
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

  /** 2D array for generating zobrist hash.
    * */
  val ztable: Array[Array[Long]] = for(x <- Array.range(0,(rows+1)*cols)) yield for(y <- Array.range(0,pieceIndices.keys.size)) yield scala.util.Random.nextLong()

  /** Returns the (blackvalue, whitevalue) pair of a list of pieces.
    * */
  def computeVals(l: List[Piece]): (Double, Double) = {
    var white = 0.0
    var black = 0.0

    for(p <- l){
      if(p.getPlayer == White())
        white = white + p.value
      else
        black = black + p.value
    }
    (black, white)
  }

  /** Returns a state from a list of strings, representing the imcs depiction
    * of the game board read in line-by-line.
    * */
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

    state
  }

  /** Returns a state from an imcs board representation contained in the file referred to by
    * the filename given.
    * */
  def stateFromFile(file: String): State = {
    val lines: Iterator[String] = Source.fromFile(file).getLines()

    Params.stringsToState(lines.toList)
  }

  /** Converts an imcs move string (e.g. a1-b2) to a Move object.
    * */
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

  /** Generates a zobrist hash (Long value) of the given state, searched to the given depth.
    * */
  def zobristHash(s: State, depth: Int): Long = {
    var h:Long = 0
    val sorted = s.pieces.sortBy(p => (p.getLoc.x, p.getLoc.y))

    for(piece <- sorted) {
      val ind = pieceIndices get piece.toString
      if(ind.isDefined)
        h = h ^ ztable(piece.getLoc.x*cols + piece.getLoc.y)(ind.get)
    }

    if(s.on_move == White()) {
      //h = h ^ whiteValue ^ depth
      h = h ^ whiteValue ^ s.moveNum
      //h = h ^ whiteValue ^ s.moveNum ^ depth
    }
    else {
      //h = h ^ blackValue ^ depth
      h = h ^ blackValue ^ s.moveNum
      //h = h ^ blackValue ^ s.moveNum ^ depth
    }
    h
  }
}

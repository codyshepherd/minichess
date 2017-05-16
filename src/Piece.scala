/** Pieces.scala
  * Cody Shepherd
  */

/** Piece is an ADT from which Pawn extends. Having the base class here
  * is a way for me to (hopefully) do less work when it comes time to
  * implement MiniChess, as I can (hopefully) simply extend the other
  * piece types from this class.
  *
  * A piece works by transforming a state with one of its move functions.
  * Its move functions are defined by name in funcList, and are stored in
  * a hash map, whose keys are the values in funcList.
  * */
abstract class Piece(p: Player, l: Loc) {
  //val funcs: Map[String, State => State]
  val funcList: List[String]
  override def toString : String
  override def equals(o: Any): Boolean
  def doMove(mv: String, s: State): State
  def getLoc: Loc = this.l
  def getPlayer: Player = this.p
  def getMovLoc(m: String): Loc
  def value : Double
  def isLegal(mv: String, s: State): Boolean
  def legalMoves(s: State): List[String]
  def isInBounds(l:Loc): Boolean = {
    if (l.x < Params.bottom || l.x > Params.top)
      false
    else if (l.y < Params.leftLimit || l.y > Params.rightLimit)
      false
    else
      true
  }

  def range(mv: String, s: State): Int = {
    for(i <- List.range(1,7)){
      val newLoc = this.getMovLoc(mv + i)

      if(!isInBounds(newLoc))
        return i-1

      val found = s.pieces.find((p: Piece) => p.getLoc == newLoc)

      if (found.isDefined) {
        if (found.get.getPlayer == this.p)
          return i - 1
        else
          return i
      }
    }
    0
  }

  def isPathClear(l: Loc, s: State): Boolean = {
    if (this.l.x == l.x && this.l.y == l.y) {
      true
    }
    else if (this.l.x == l.x) {
      for (i <- List.range(math.min(this.l.y, l.y) + 1, math.max(this.l.y, l.y), 1)) {
        if (s.pieces.exists((p: Piece) => p.getLoc == (this.l.x, i))) {
          return false
        }
      }
      true
    }
    else if (this.l.y == l.y) {
      for(piece <- s.pieces)
      for (i <- List.range(math.min(this.l.x, l.x) + 1, math.max(this.l.x, l.x), 1)) {
        if (s.pieces.exists((p: Piece) => p.getLoc == (i, this.l.y))) {
          return false
        }
      }
      true
    }
    else {
      val ydiff = l.y - this.l.y
      val xdiff = l.x - this.l.x

      val cols = ydiff match {
        case yd if yd == 0 => List.fill(math.abs(xdiff - 1))(l.y)
        case yd if yd > 0 => List.range(this.l.y + 1, l.y)
        case yd if yd < 0 => List.range(this.l.y - 1, l.y, -1)
      }

      val rows = xdiff match {
        case xd if xd == 0 => List.fill(math.abs(ydiff - 1))(l.x)
        case xd if xd > 0 => List.range(this.l.x + 1, l.x)
        case xd if xd < 0 => List.range(this.l.x - 1, l.x, -1)
      }

      val path = rows zip cols

      for (step <- path) {
        if (s.pieces.exists((p: Piece) => p.getLoc.x == step._1 && p.getLoc.y == step._2)) {
          return false
        }
      }
      true
    }
  }
}
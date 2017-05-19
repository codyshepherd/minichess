/** Bishop.scala
  * minichess
  * Cody Shepherd
  * */
import scala.collection.mutable.ListBuffer

/** The Bishop class represents the Bishop piece. It overrides the Piece.isLegal function
  * because it has the special up-down-left-right non-capturing moves, which are special
  * cases.
  */
case class Bishop(p: Player, var l: Loc) extends Piece(p,l){
  val funcList = List("fwdRight", "fwdLeft", "bakRight", "bakLeft", "fwd1", "bak1", "left1", "right1")
  val altFuncList = List("fwd1", "bak1", "left1", "right1")

  def value : Double = 3.0

  def getMe(nl: Loc): Bishop = {Bishop(p, nl)}

  override def toString : String = {
    p match {
      case Black() => "b"
      case White() => "B"
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case that: Bishop =>
        if (this.p == that.p && this.l == that.l)
          true
        else
          false
      case _ => false
    }
  }

  /** Returns the board locataion given a move-string from
    * the funcsList.
    * */
  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toString.toInt
    var mov: String = ""

    if(altFuncList.contains(m))
      mov = m
    else
      mov = m.init

    assert(funcList.contains(mov))

    mov match {
      case "fwd1" => new Loc(x = this.l.x + p.op(1), y = this.l.y)
      case "bak1" => new Loc(x = this.l.x - p.op(1), y = this.l.y)
      case "left1" => new Loc(x = this.l.x, y = this.l.y - 1)
      case "right1" => new Loc(x = this.l.x, y = this.l.y + 1)
      case "fwdLeft" => new Loc(x = this.l.x + this.p.op(nToMov), y = this.l.y - nToMov)
      case "fwdRight" => new Loc(x = this.l.x + this.p.op(nToMov), y = this.l.y + nToMov)
      case "bakLeft" => new Loc(x = this.l.x - this.p.op(nToMov), y = this.l.y - nToMov)
      case "bakRight" => new Loc(x = this.l.x - this.p.op(nToMov), y = this.l.y + nToMov)

    }
  }

  /** Returns whether this bishop can make the given move (from the funcList) within
    * the given state.
    * */
 override def isLegal(mv: String, s: State): Boolean = {
    val move = mv.init
    if (!funcList.contains(move) && !funcList.contains(mv)) {
      return false
    }

    val newLoc = getMovLoc(mv)

    if (!isInBounds(newLoc)){
      return false
    }

    val maybePiece = s.pieces.find((p: Piece) => p.getLoc == newLoc)
    if(maybePiece.isDefined) {
      if (altFuncList.contains(mv)) { // if there is a piece there and Bishop is doing non-capping move, say no
        false
      }
      else { // otherwise, capping move
        if (maybePiece.get.getPlayer == this.p.opposite)
          true // as long as that piece is the opponent's, sure
        else false // if the piece there is our piece, then no
      }
    }
    else true                       // otherwise that location is empty, so yes, going there is legal
  }

  /** Generates all legal moves that can be made by this piece within the given state.
    * */
  def legalMoves(s: State): List[String] = {
    var moves: ListBuffer[String] = ListBuffer()
    for(move <- funcList) {
      if(altFuncList.contains(move)){
        if(isLegal(move, s))
          moves += move
      }
      else{
        for(i <- List.range(1,range(move, s) + 1)){
          if(isLegal(move+ i.toString, s))
            moves += move + i.toString
        }
      }

    }
    moves.toList
  }
}

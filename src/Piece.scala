/** Piece.scala
  * minichess
  * Cody Shepherd
  */

/** Piece is the ADT from which all pieces extend.
  *
  * A piece works by transforming a state with one of its move functions.
  * Its possible moves are defined by name in funcList.
  * */
abstract class Piece(p: Player, l: Loc) {
  val funcList: List[String]
  override def toString : String
  def getMe(l: Loc): Piece
  override def equals(o: Any): Boolean

  /** Returns the state produced by executing the given move on the given state.
    * */
  def doMove(mv: String, s: State): State = {
    val newLoc = getMovLoc(mv)

    val found = s.pieces.find(p => p.getLoc == newLoc)

    val newp = getMe(newLoc)
    if(found.isDefined){
      new State( this.p.opposite,
        if(this.p == Black()) s.moveNum + 1 else s.moveNum,
        if(this.p == Black()) s.b_value else s.b_value - found.get.value,
        if(this.p == White()) s.w_value else s.w_value - found.get.value,
        newp :: s.pieces.par.filterNot(p => p.getLoc == this.l || p.getLoc == found.get.getLoc).toList
      )
    }
    else{
      new State( this.p.opposite,
        if(this.p == Black()) s.moveNum + 1 else s.moveNum,
        s.b_value,
        s.w_value,
        newp :: s.pieces.par.filterNot(p => p.getLoc == this.l).toList
      )

    }
  }
  def getLoc: Loc = this.l
  def getPlayer: Player = this.p
  def getMovLoc(m: String): Loc
  def value : Double

  /** Returns whether the given move (a string from a piece's funcList) is
    * legal within the given state.
    * */
  def isLegal(mv: String, s: State): Boolean = {
    if (!funcList.contains(mv.init))
      return false

    val lengthPossible = range(mv.init, s)

    if(mv.last.toString.toInt > lengthPossible)
      return false

    val newLoc = getMovLoc(mv)
    if(!isInBounds(newLoc))
      return false

    val found = s.pieces.find(p => p.getLoc == newLoc)

    if(found.isDefined){
      if(found.get.getPlayer == this.p)
        return false
      else
        return true
    }
    true
  }

  def legalMoves(s: State): List[String]

  /** Returns whether or not the location is on the board.
    * */
  def isInBounds(l:Loc): Boolean = {
    if (l.x < Params.bottom || l.x > Params.top)
      false
    else if (l.y < Params.leftLimit || l.y > Params.rightLimit)
      false
    else
      true
  }

  /** Returns the farthest number of squares a piece can legally move (to include
    * capture), given the move string and the state.
    * */
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

}
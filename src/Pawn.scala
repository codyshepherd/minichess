/**
  * Created by cody on 4/26/17.
  */



/** Pawn is the basic piece, obviously.
  *
  * It can do one of three things under appropriate conditions:
  * move forward, capture to the right, or capture to the left.
  * */
case class Pawn(p: Player, var l: Loc) extends Piece(p,l) {

  def value : Double = 1.0

  def getMe(nl: Loc): Pawn = {Pawn(p, nl)}

  /** funcs is a hash map of all the movement functions this piece can perform.
    * By convention its members should only be accessed through the items in
    * this piece's funcList. This will allow generating a piece's moves to be
    * at once constrained and programmatic.
    *
    * This member should probably be private?
    * */
  val funcs = Map(
    "fwd" -> PartialFunction(fwd),
    "capRight" -> PartialFunction(capRight),
    "capLeft" -> PartialFunction(capLeft)
  )

  /** The funcList holds the names of all this piece's movement functions.
    * */
  val funcList:List[String] = funcs.keys.toList

  override def toString: String = {
    p match {
      case Black() => "p"
      case White() => "P"
    }
  }

  /** I needed a special kind of equivalence checking
    * */
  override def equals(o:Any): Boolean = {
    o match {

      case that: Pawn =>
        if (this.p == that.p && this.l == that.l)
          true
        else
          false

      case _ => false
    }
  }

  override def doMove(mv: String, s: State): State = {
    funcs(mv)(s)
  }

  /** Returns the state derived when this piece moves forward, given the argument
    * state.
    *
    * Assumes that forward has already been deemed a legal move.
    *
    * (Should probably figure
    * out a way to handle some possibility where this piece does not exist in the
    * given state)
    *
    * Pawns have the special case that they could be promoted
    * */
  def fwd(s: State): State = {
    val newLoc = getMovLoc("fwd")

    val found = s.pieces.find(p => p.getLoc == newLoc)
    assert(found.isEmpty) // if this assertion fails, this moved was deemed legal and actually is not

    var newp: Piece = this

    if ((this.p == White() && newLoc.x == Params.top) || (this.p == Black() && newLoc.x == Params.bottom))
      newp = Queen(this.p, newLoc)
    else
      newp = Pawn(this.p, newLoc)

    new State(this.p.opposite,
      if (this.p == Black()) s.moveNum + 1 else s.moveNum,
      if (this.p == Black()) s.b_value - 1 + newp.value else s.b_value,
      if (this.p == White()) s.w_value - 1 + newp.value else s.w_value,
      newp :: s.pieces.filterNot(p => p.getLoc == this.l)
    )
  }

  /** Returns the state derived when this piece captures to the right; i.e. captures, and
    * the captured piece is at a higher column than this piece, irregardless of the color
    * of either piece.
    *
    * Assumes capturing to the right has already been deemed legal.
    *
    * XXX: capRight and capLeft are now identical functions, except for the string passed to
    * getMovLoc(). I wonder if it would be worthwhile to consolidate them (probably)
    * */
  def capRight(s: State): State = {
    val newLoc = getMovLoc("capRight")

    val found = s.pieces.find(p => p.getLoc == newLoc)
    assert(found.isDefined) // if this assertion fails, this moved was deemed legal and actually is not

    var newp: Piece = this

    if ((this.p == White() && newLoc.x == Params.top) || (this.p == Black() && newLoc.x == Params.bottom))
      newp = Queen(this.p, newLoc)
    else
      newp = Pawn(this.p, newLoc)

    new State(this.p.opposite,
      if (this.p == Black()) s.moveNum + 1 else s.moveNum,
      if (this.p == Black()) s.b_value - 1 + newp.value else s.b_value - found.get.value,
      if (this.p == White()) s.w_value - 1 + newp.value else s.w_value - found.get.value,
      newp :: s.pieces.filterNot(p => p.getLoc == this.l || p.getLoc == found.get.getLoc)
    )
  }

  /** Returns the state derived when this piece captures to the left; i.e. captures,
    * and the captured piece is at a lower column than this piece, irregardless of the
    * color of either piece.
    *
    * Assumes capturing to the left has already been deemed legal.
    * */
  def capLeft(s: State): State = {
    val newLoc = getMovLoc("capLeft")

    val found = s.pieces.find(p => p.getLoc == newLoc)
    assert(found.isDefined)   // if this assertion fails, this moved was deemed legal and actually is not

    var newp: Piece = this

    if((this.p == White() && newLoc.x == Params.top) || (this.p == Black() && newLoc.x == Params.bottom))
      newp = Queen(this.p, newLoc)
    else
      newp = Pawn(this.p, newLoc)

    new State( this.p.opposite,
      if(this.p == Black()) s.moveNum + 1 else s.moveNum,
      if(this.p == Black()) s.b_value - 1 + newp.value else s.b_value - found.get.value,
      if(this.p == White()) s.w_value - 1 + newp.value else s.w_value - found.get.value,
      newp :: s.pieces.filterNot(p => p.getLoc == this.l || p.getLoc == found.get.getLoc)
    )
  }

  /** Returns the location (coordinates) on which this piece would end up if it performed
    * the given move.
    *
    * This method supports checking whether a move is legal.
    * */
  def getMovLoc(m: String): Loc = {
    val newx = this.l.x + this.p.op(1)

    //System.err.println("Pawn")
    //System.err.println("m: " + m)
    //System.err.println("nToMov: " + nToMov)
    //System.err.println("mov: " + mov)

    m match {
      case "fwd" => new Loc(x = newx, y = this.l.y)
      case "capRight" => new Loc(x = newx, y = this.l.y + 1)
      case "capLeft" => new Loc(x = newx, y = this.l.y - 1 )
    }
  }

  override def isLegal(mv: String, s: State): Boolean = {
    if (!funcList.contains(mv))
      return false

    val newLoc = getMovLoc(mv)

    if (!isInBounds(newLoc))
      return false

    val found = s.pieces.find(p => p.getLoc == newLoc)

    if(mv == "capLeft" || mv == "capRight"){
      if(found.isEmpty || found.get.getPlayer == this.p)
        return false
    }
    else{
      if(found.isDefined)
        return false
    }
    true
  }

  def legalMoves(s: State): List[String] = {
    var moves: List[String] = List()
    for(move <- funcList){
      if(isLegal(move,s))
        moves = move :: moves
    }
    moves
  }

}

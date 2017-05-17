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
  def fwd(st: State): State = {
    val np = st.pieces.filterNot((x: Piece) => x == this)

    assert(np != st.pieces)
    val newLoc = getMovLoc("fwd")
    val addMe = Params.queen - 1.0

    this.p match {

      case Black() =>
        //check for promotion
        if(newLoc.x == Params.bottom) {
          // if promoted, make a queen
          val newq = Queen(this.p, l = newLoc)
          // add back the queen, modifying this player's value
          // Only necessary to explicitly increment move when it becomes white's turn
          new State(on_move = this.p.opposite, moveNum = st.moveNum+1, b_value = st.b_value + addMe, w_value = st.w_value, pieces = newq :: np)
        }
        else {
          // not promoted, make a pawn
          val newp = Pawn(p = this.p, l = newLoc)
          // add back the pawn, no change to the value of either state
          // Only necessary to explicitly increment move when it becomes white's turn
          new State(on_move = this.p.opposite, moveNum = st.moveNum+1, b_value = st.b_value, w_value = st.w_value, pieces = newp :: np)
        }

      case White() =>
        //check for promotion
        if(newLoc.x == Params.top) {
          // if promoted, make a queen
          val newq = Queen(this.p, l = newLoc)
          // return new state, adding the queen and modifying this player's value
          // making black's move number match whites will implicitly increment it
          new State(on_move = this.p.opposite, moveNum = st.moveNum, b_value = st.b_value, w_value = st.w_value + addMe, pieces = newq :: np )
        }
        else {
          // if not promoted, make a pawn
          val newp = Pawn(p = this.p, l = newLoc)
          // add back the pawn, no change to value of either side
          // making black's move number match whites will implicitly increment it
          new State(on_move = this.p.opposite, moveNum = st.moveNum, b_value = st.b_value, w_value = st.w_value, pieces = newp :: np)
        }
    }
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
  def capRight(st: State): State = {
    val newLoc = getMovLoc("capRight")                              // the new location the piece will move to
    val removeMe = (x: Piece) => x == this
    val removeCap = (mp: Piece) => mp.getLoc == newLoc

    val capped = st.pieces.filter(removeCap)                        // the piece that will be captured
    assert(capped.length == 1)
    val subMe = capped(0).value                                     // the value of the captured piece
    val addMe = Params.queen - 1.0                                  // the added value of a queen over a pawn
    val np = st.pieces.filterNot(removeMe).filterNot(removeCap)     // the pieces of this state without the moved piece
    assert(np != st.pieces)

    this.p match {

      case Black() =>
        //check for Promotion
        if(newLoc.x == Params.bottom){
          // if promoted, make the new piece a queen
          val newq = Queen(this.p, l = newLoc)
          // return the new state, modifying the values for both sides
          // Only necessary to explicitly increment move when it becomes white's turn
          new State(on_move = this.p.opposite, moveNum = st.moveNum+1, b_value = st.b_value + addMe, w_value = st.w_value - subMe, pieces = newq :: np)
        }
        else {
          // not promoted, so just make a pawn with the new location
          val newp = Pawn(p = this.p, l = newLoc)
          // return the new state, modifying only the opponent's value
          // Only necessary to explicitly increment move when it becomes white's turn
          new State(on_move = this.p.opposite, moveNum = st.moveNum+1, b_value = st.b_value, w_value = st.w_value - subMe, pieces = newp :: np)
        }

      case White() =>
        // check for promotion
        if(newLoc.x == Params.top){
          // if promoted, make the new piece a queen
          val newq = Queen(this.p, l = newLoc)
          // return the new state, modifying the values for both sides
          // making black's move number match whites will implicitly increment it
          new State(on_move = this.p.opposite, moveNum = st.moveNum, b_value = st.b_value - subMe, w_value = st.w_value + addMe, pieces = newq :: np)
        }
        else {
          // if not promoted, make a pawn
          val newp = Pawn(p = this.p, l = newLoc)
          // add back the pawn, modify just the opponent' value
          // making black's move number match whites will implicitly increment it
          new State(on_move = this.p.opposite, moveNum = st.moveNum, b_value = st.b_value - subMe, w_value = st.w_value, pieces = newp :: np)
        }
    }
  }

  /** Returns the state derived when this piece captures to the left; i.e. captures,
    * and the captured piece is at a lower column than this piece, irregardless of the
    * color of either piece.
    *
    * Assumes capturing to the left has already been deemed legal.
    * */
  def capLeft(st: State): State = {
    val newLoc = getMovLoc("capLeft")                              // the new location the piece will move to

    val removeMe = (x: Piece) => x == this
    val removeCap = (mp: Piece) => mp.getLoc == newLoc

    val capped = st.pieces.filter(removeCap)                        // the piece that will be captured
    assert(capped.length == 1)
    val subMe = capped(0).value                                     // the value of the captured piece
    val addMe = Params.queen - 1.0                                  // the added value of a queen over a pawn
    val np = st.pieces.filterNot(removeMe).filterNot(removeCap)     // the pieces of this state without the moved piece
    assert(np != st.pieces)

    this.p match {

      case Black() =>
        //check for Promotion
        if(newLoc.x == Params.bottom){
          // if promoted, make the new piece a queen
          val newq = Queen(this.p, l = newLoc)
          // return the new state, modifying the values for both sides
          // Only necessary to explicitly increment move when it becomes white's turn
          new State(on_move = this.p.opposite, moveNum = st.moveNum+1, b_value = st.b_value + addMe, w_value = st.w_value - subMe, pieces = newq :: np)
        }
        else {
          // not promoted, so just make a pawn with the new location
          val newp = Pawn(p = this.p, l = newLoc)
          // return the new state, modifying only the opponent's value
          // Only necessary to explicitly increment move when it becomes white's turn
          new State(on_move = this.p.opposite, moveNum = st.moveNum+1, b_value = st.b_value, w_value = st.w_value - subMe, pieces = newp :: np)
        }

      case White() =>
        // check for promotion
        if(newLoc.x == Params.top){
          // if promoted, make the new piece a queen
          val newq = Queen(this.p, l = newLoc)
          // return the new state, modifying the values for both sides
          // making black's move number match whites will implicitly increment it
          new State(on_move = this.p.opposite, moveNum = st.moveNum, b_value = st.b_value - subMe, w_value = st.w_value + addMe, pieces = newq :: np)
        }
        else {
          // if not promoted, make a pawn
          val newp = Pawn(p = this.p, l = newLoc)
          // add back the pawn, modify just the opponent' value
          // making black's move number match whites will implicitly increment it
          new State(on_move = this.p.opposite, moveNum = st.moveNum, b_value = st.b_value - subMe, w_value = st.w_value, pieces = newp :: np)
        }
    }

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

  def isLegal(mv: String, s: State): Boolean = {
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

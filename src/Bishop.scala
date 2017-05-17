import scala.collection.mutable.ListBuffer

/**
  * Created by cody on 4/26/17.
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

  /** Assumes move has already been checked as legal
    * */
  /*
  def doMove(mv: String, s: State): State = {
    val newLoc = getMovLoc(mv)

    val np = s.pieces.filterNot((x: Piece) => x == this) // get all pieces but this one
    assert(np != s.pieces)

    val capped = s.pieces.filter((x: Piece) => x.getLoc == newLoc) // get the capped piece, if there is one
    assert(capped.length <= 1)
    val cappedPiece = capped.headOption

    val newq = Bishop(this.p, l = newLoc)  // make a new piece at the ending location

    capped.length match { // check if there was a piece that got captured

      // no items found means no capture to worry about; just add the new queen back at the new location
      case 0 =>
        new State(on_move = this.p.opposite, moveNum = if(this.p == Black()) s.moveNum+1 else s.moveNum, b_value = s.b_value, w_value = s.w_value, pieces = newq :: np)

      // we've already asserted that the length is upper bounded by 1; this case represents a capture
      case _ =>
        val nnp = np.filterNot((x: Piece) => cappedPiece.isDefined && x == cappedPiece.get)
        if(this.p == White())
          new State(on_move = this.p.opposite,
            moveNum = s.moveNum,
            b_value = if(cappedPiece.isDefined) s.b_value - cappedPiece.get.value else s.b_value,
            w_value = s.w_value,
            pieces = newq :: nnp )// update black's movenum to match this one, subtract value of
            // capped piece from black
        else
          new State(on_move = this.p.opposite,
            moveNum = s.moveNum+1,
            b_value = s.b_value,
            w_value = if(cappedPiece.isDefined) s.w_value - cappedPiece.get.value else s.w_value,
            pieces = newq :: nnp)// update white's movenum (increment it),
            // and subtract value of capped piece from white
    }
  }
  */

  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toString.toInt
    var mov: String = ""

    if(altFuncList.contains(m))
      mov = m
    else
      mov = m.init

    /*
    System.err.println("Bishop")
    System.err.println("m: " + m)
    System.err.println("nToMov: " + nToMov)
    System.err.println("mov: " + mov)
    */

    if (!funcList.contains(mov))
      return new Loc(-1, -1)
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

  def isLegal(mv: String, s: State): Boolean = {
    val move = mv.init
    if (!funcList.contains(move) && !funcList.contains(mv)) {
      return false
    }

    val newLoc = getMovLoc(mv)

    //if (!isInBounds(newLoc) || !isPathClear(newLoc, s))
    if (!isInBounds(newLoc)){
      return false
    }

    val maybePiece = s.pieces.find((p: Piece) => p.getLoc == newLoc)
    maybePiece match {
      case Some(a) => if(altFuncList.contains(mv)){   // if there is a piece there and Bishop is doing non-capping move, say no
        false
      }
      else                                            // otherwise, capping move
        a.getPlayer match {
          case c if c == this.p.opposite => true        // as long as that piece is the opponent's, sure
          case _ => false                     // if the piece there is our piece, then no
        }
      case None => true                       // otherwise that location is empty, so yes, going there is legal
    }
  }

  def movesWhilePathClear(mv: String, s: State): List[String] = {
    var moves: List[String] = List()
    for (i <- List.range(1,Params.rows)){
      val move = mv + i.toString
      if (!isLegal(move, s))
        return moves
      else
        moves = move :: moves
    }
    moves
  }

  def legalMoves(s: State): List[String] = {
    var moves: ListBuffer[String] = ListBuffer()
    for(move <- funcList) {
      if(altFuncList.contains(move)){
        if(isLegal(move, s))
          moves += move
      }
      else{
        for(i <- List.range(1,range(move, s) + 1)){
          //if(isLegal(move+1, s))
            moves += move + i
        }
      }

    }
    moves.toList
  }
}

/**
  * Created by cody on 4/26/17.
  */
case class Knight(p: Player, l: Loc) extends Piece(p,l){

  val funcList = List("longLeft", "longRight", "shortLeft", "shortRight")

  def value: Double = 3.0

  override def toString : String = {
    p match {
      case Black() => "n"
      case White() => "N"
    }
  }
  override def equals(o: Any): Boolean = {
    o match {
      case that: Knight =>
        if (this.p == that.p && this.l == that.l)
          true
        else
          false
      case _ => false
    }
  }

  override def isPathClear(l: Loc, s: State): Boolean = true

  /** Assumes the move has already been deemed legal.
    *
    *
    * */
  def doMove(mv: String, s: State): State = {
    val newLoc = getMovLoc(mv)

    val np = s.pieces.filterNot((x: Piece) => x == this)
    assert(np != s.pieces)

    val capped = s.pieces.filter((x: Piece) => x.getLoc == newLoc)
    assert(capped.length <= 1)

    val cappedPiece = capped.headOption

    val newq = Knight(this.p, l = newLoc)

    capped.length match {

      // no items found means no capture to worry about; just add the new queen back at the new location
      case 0 =>
        new State(on_move = this.p.opposite, moveNum = if(p == White()) s.moveNum+1 else s.moveNum, b_value = s.b_value, w_value = s.w_value, pieces = newq :: np)

      // we've already asserted that the length is upper bounded by 1; this case represents a capture
      case _ =>
        val nnp = np.filterNot((x: Piece) => cappedPiece.isDefined && x == cappedPiece.get)  // remove capped piece from the board
        if(this.p == White())
          new State(on_move = this.p.opposite, moveNum = s.moveNum,c b_value = if(cappedPiece.isDefined) s.b_value - cappedPiece.get.value else s.b_value,
            w_value = s.w_value, pieces = newq :: nnp) // update black's movenum to match this one, subtract value of
        // capped piece from black
        else
          new State(on_move = this.p.opposite, moveNum = s.moveNum+1, b_value = s.b_value,
            w_value = if(cappedPiece.isDefined) s.w_value - cappedPiece.get.value else s.w_value, pieces = newq :: nnp)  // update white's movenum (increment it),
      // and subtract value of capped piece from white
    }
  }

  /** Assumes that the number to move  has been tacked on to the end of the move string as a numeric character
    * */
  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toString.toInt
    val mov: String = m.init

    /*
    System.err.println("Knight")
    System.err.println("m: " + m)
    System.err.println("nToMov: " + nToMov)
    System.err.println("mov: " + mov)
    */

    if (!funcList.contains(mov))
      return new Loc(-1,-1)
    mov match {
      case "longLeft" => nToMov match {
        case 1 => new Loc(x = this.l.x + this.p.op(2), y = this.l.y - 1) // case 1 represents "forward"
        case 2 => new Loc(x = this.l.x + this.p.op(1), y = this.l.y + 2) // case 2 represents "right"
        case 3 => new Loc(x = this.l.x - this.p.op(2), y = this.l.y + 1) // case 3 represents "back"
        case 4 => new Loc(x = this.l.x - this.p.op(1), y = this.l.y - 2) // case 4 represents "left"
      }
      case "longRight" => nToMov match {
        case 1 => new Loc(x = this.l.x + this.p.op(2), y = this.l.y + 1) // case 1 represents "forward"
        case 2 => new Loc(x = this.l.x - this.p.op(1), y = this.l.y + 2) // case 2 represents "right"
        case 3 => new Loc(x = this.l.x - this.p.op(2), y = this.l.y - 1) // case 3 represents "back"
        case 4 => new Loc(x = this.l.x + this.p.op(1), y = this.l.y - 2) // case 4 represents "left"
      }
      case "shortLeft" => nToMov match {
        case 1 => new Loc(x = this.l.x + this.p.op(1), y = this.l.y - 2) // case 1 represents "forward"
        case 2 => new Loc(x = this.l.x + this.p.op(2), y = this.l.y + 1) // case 2 represents "right"
        case 3 => new Loc(x = this.l.x - this.p.op(1), y = this.l.y + 2) // case 3 represents "back"
        case 4 => new Loc(x = this.l.x - this.p.op(2), y = this.l.y - 1) // case 4 represents "left"
      }
      case "shortRight" => nToMov match {
        case 1 => new Loc(x = this.l.x + this.p.op(1), y = this.l.y + 2) // case 1 represents "forward"
        case 2 => new Loc(x = this.l.x - this.p.op(2), y = this.l.y + 1) // case 2 represents "right"
        case 3 => new Loc(x = this.l.x - this.p.op(1), y = this.l.y - 2) // case 3 represents "back"
        case 4 => new Loc(x = this.l.x + this.p.op(2), y = this.l.y - 1) // case 4 represents "left"
      }

    }
  }

  def isLegal(mv: String, s: State): Boolean = {
    val move = mv.init
    if (!funcList.contains(move))
      return false

    val newLoc = getMovLoc(mv)

    if (!isInBounds(newLoc))
      return false

    val maybePiece = s.pieces.find((p: Piece) => p.getLoc == newLoc)
    maybePiece match {
      case Some(a) => a.getPlayer match {   // if there is a piece at the new location
        case c if c == this.p.opposite => true        // as long as that piece is the opponent's, sure
        case _ => false                     // if the piece there is our piece, then no
      }
      case None => true                     // we can move to an blank space we can reach
    }
  }

  def legalMoves(s: State): List[String] = {
    var moves: List[String] = List()
    for(move <- funcList){
      for (i <- List.range(1,5)){       // every knight move takes a number between 1 and 4 (inclusive) as argument
        val mv = move + i.toString
        if (isLegal(mv, s))
          moves = mv :: moves
      }
    }
    moves.distinct
  }
}


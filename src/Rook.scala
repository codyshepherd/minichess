/**
  * Created by cody on 4/26/17.
  */
case class Rook(p: Player, l: Loc) extends Piece(p,l){
  val funcList = List("fwd", "bak", "left", "right")

  def value : Double = 5.0

  override def toString : String = {
    p match {
      case Black() => "r"
      case White() => "R"
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case that: Rook =>
        if (this.p == that.p && this.l == that.l)
          true
        else
          false
      case _ => false
    }
  }

  /** Assumes move has already been checked as legal
    * */
  def doMove(mv: String, s: State): State = {
    val newLoc = getMovLoc(mv)

    val np = s.pieces.filterNot((x: Piece) => x == this) // get all pieces but this one
    assert(np != s.pieces)

    val capped = s.pieces.filter((x: Piece) => x.getLoc == newLoc) // get the capped piece, if there is one
    assert(capped.length <= 1)
    val cappedPiece = capped.head

    val newq = Rook(this.p, l = newLoc)  // make a new piece at the ending location

    capped.length match { // check if there was a piece that got captured

      // no items found means no capture to worry about; just add the new queen back at the new location
      case 0 =>
        new State(on_move = this.p.opposite, moveNum = if(this.p == Black()) s.moveNum+1 else s.moveNum, b_value = s.b_value, w_value = s.w_value, pieces = np :+ newq)

      // we've already asserted that the length is upper bounded by 1; this case represents a capture
      case _ =>
        val nnp = np.filterNot((x: Piece) => x == cappedPiece)
        if(this.p == White())
          new State(on_move = this.p.opposite, moveNum = s.moveNum, b_value = s.b_value - cappedPiece.value,
            w_value = s.w_value, pieces = nnp :+ newq)// update black's movenum to match this one, subtract value of
            // capped piece from black
        else
          new State(on_move = this.p.opposite, moveNum = s.moveNum+1, b_value = s.b_value,
            w_value = s.w_value - cappedPiece.value, pieces = nnp :+ newq)// update white's movenum (increment it),
            // and subtract value of capped piece from white
    }
  }

  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toInt
    val mov: String = m.init
    if (!funcList.contains(mov))
      return new Loc(-1, -1)
    mov match {
      case "fwd" => new Loc(x = this.l.x + this.p.op(nToMov), y = this.l.y)
      case "bak" => new Loc(x = this.l.x - this.p.op(nToMov), y = this.l.y)
      case "left" => new Loc(x = this.l.x, y = this.l.y - nToMov)
      case "right" => new Loc(x = this.l.x, y = this.l.y + nToMov)
    }
  }

}


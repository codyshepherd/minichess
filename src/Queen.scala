/**
  * Created by cody on 4/26/17.
  */
case class Queen(p: Player, l: Loc) extends Piece(p,l){

  val funcList = List("fwd", "bak", "fwdLeft", "fwdRight", "bakLeft", "bakRight")

  def value: Double = 9.0

  override def toString : String = {
    p match {
      case Black() => "q"
      case White() => "Q"
    }
  }
  override def equals(o: Any): Boolean = {
    o match {
      case that: Queen =>
        if (this.p == that.p && this.l == that.l)
          true
        else
          false
      case _ => false
    }
  }

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
    val cappedPiece = capped.head

    val newq = Queen(this.p, l = newLoc)

    capped.length match {

      // no items found means no capture to worry about; just add the new queen back at the new location
      case 0 =>
        new State(on_move = this.p.opposite, moveNum = if(p == White()) s.moveNum+1 else s.moveNum, b_value = s.b_value, w_value = s.w_value, pieces = np :+ newq)

      // we've already asserted that the length is upper bounded by 1; this case represents a capture
      case _ =>
        val nnp = np.filterNot((x: Piece) => x == cappedPiece)
        if(this.p == White())
          new State(on_move = this.p.opposite, moveNum = if(p == White()) s.moveNum+1 else s.moveNum, b_value = s.b_value - cappedPiece.value, w_value = s.w_value, pieces = nnp :+ newq)
        else
          new State(on_move = this.p.opposite, moveNum = if(p == White()) s.moveNum+1 else s.moveNum, b_value = s.b_value, w_value = s.w_value - cappedPiece.value, pieces = nnp :+ newq)
    }
  }

  /** Assumes that the number to move  has been tacked on to the end of the move string as a numeric character
    * */
  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toInt
    val mov: String = m.init
    mov match {
      case "fwd" => new Loc(x = this.l.x + (this.p.op(1) * nToMov), y = this.l.y)
      case "bak" => new Loc(x = this.l.x - (this.p.op(1) * nToMov), y = this.l.y)
      case "fwdLeft" => new Loc(x = this.l.x + (this.p.op(1) * nToMov), y = this.l.y - nToMov)
      case "fwdRight" => new Loc(x = this.l.x + (this.p.op(1) * nToMov), y = this.l.y + nToMov)
      case "bakLeft" => new Loc(x = this.l.x - (this.p.op(1) * nToMov), y = this.l.y - nToMov)
      case "bakRight" => new Loc(x = this.l.x - (this.p.op(1) * nToMov), y = this.l.y + nToMov)

    }
  }
}

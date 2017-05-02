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

  def isLegal(mv: String, s: State): Boolean = {
    val move = mv.init
    if (!funcList.contains(move))
      return false

    val newLoc = getMovLoc(mv)

    if (!isInBounds(newLoc))
      return false

    val ydiff = newLoc.y - this.l.y
    val xdiff = newLoc.x - this.l.x

    val cols = ydiff match {
      case yd if yd == 0 => List.fill(math.abs(xdiff))(newLoc.y)
      case yd if yd > 0 => List.range(this.l.y+1, newLoc.y)
      case yd if yd < 0 => List.range(newLoc.y+1, this.l.y)
    }

    val rows = xdiff match {
      case xd if xd == 0 => List.fill(math.abs(ydiff))(newLoc.x)
      case xd if xd > 0 => List.range(this.l.x+1, newLoc.x)
      case xd if xd < 0 => List.range(newLoc.x+1, this.l.x)
    }

    val path = rows zip cols

    for(location <- path){
      if (s.pieces.exists((p: Piece) => p.getLoc == new Loc(location._1, location._2)))
        return false
    }

    val maybePiece = s.pieces.find((p: Piece) => p.getLoc == newLoc)
    maybePiece match {
      case Some(a) => a.getPlayer match {   // if there is a piece at the new location
        case this.p.opposite => true        // as long as that piece is the opponent's, sure
        case _ => false                     // if the piece there is our piece, then no
      }
      case None => true                     // we can move into check, so, sure
    }
  }

  def movesWhilePathClear(mv: String, s: State): List[String] = {
    var moves: List[String] = List()
    for (i <- List.range(1,Params.rows)){
      val move = mv + i.toString
      if (!isLegal(move, s))
        return moves
      else
        moves = moves :+ move
    }
    moves
  }

  def legalMoves(s: State): List[String] = {
    var moves: List[String] = List()
    for(move <- funcList)
      moves = moves ++ movesWhilePathClear(move, s)
    moves
  }
}


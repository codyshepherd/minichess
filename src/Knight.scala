import scala.collection.mutable.ListBuffer

/**
  * Created by cody on 4/26/17.
  */
case class Knight(p: Player, var l: Loc) extends Piece(p,l){

  val funcList = List("longLeft", "longRight")

  def value: Double = 3.0

  def getMe(nl: Loc): Knight = {Knight(p, nl)}

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

  override def isLegal(mv: String, s: State): Boolean = {
    if (!funcList.contains(mv.init))
      return false

    if(mv.last.toString.toInt > 4)
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
  /** Assumes that the number to move  has been tacked on to the end of the move string as a numeric character
    * */
  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toString.toInt
    val mov: String = m.init

    assert(funcList.contains(mov))

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

    }
  }

  def legalMoves(s: State): List[String] = {
    var moves: ListBuffer[String] = ListBuffer()
    for(move <- funcList){
      for (i <- List.range(1,5)){       // every knight move takes a number between 1 and 4 (inclusive) as argument
        if (isLegal(move + i.toString, s))
          moves += move + i.toString
      }
    }
    moves.toList
  }
}


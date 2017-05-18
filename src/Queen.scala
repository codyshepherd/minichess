import scala.collection.mutable.ListBuffer

/**
  * Created by cody on 4/26/17.
  */
case class Queen(p: Player, var l: Loc) extends Piece(p,l){

  val funcList = List("fwd", "bak", "left", "right", "fwdLeft", "fwdRight", "bakLeft", "bakRight")

  def value: Double = 9.0

  def getMe(nl: Loc): Queen = {Queen(p, nl)}

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

  /** Assumes that the number to move  has been tacked on to the end of the move string as a numeric character
    * */
  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toString.toInt
    val mov: String = m.init

    assert(funcList.contains(mov))

    /*
    System.err.println("Queen")
    System.err.println("m: " + m)
    System.err.println("nToMov: " + nToMov)
    System.err.println("mov: " + mov)
    */

    mov match {
      case "fwd" => new Loc(x = this.l.x + this.p.op(nToMov), y = this.l.y)
      case "bak" => new Loc(x = this.l.x - this.p.op(nToMov), y = this.l.y)
      case "left" => new Loc(x = this.l.x, y = this.l.y - nToMov)
      case "right" => new Loc(x = this.l.x, y = this.l.y + nToMov)
      case "fwdLeft" => new Loc(x = this.l.x + this.p.op(nToMov), y = this.l.y - nToMov)
      case "fwdRight" => new Loc(x = this.l.x + this.p.op(nToMov), y = this.l.y + nToMov)
      case "bakLeft" => new Loc(x = this.l.x - this.p.op(nToMov), y = this.l.y - nToMov)
      case "bakRight" => new Loc(x = this.l.x - this.p.op(nToMov), y = this.l.y + nToMov)

    }
  }

  def legalMoves(s: State): List[String] = {
    var moves: ListBuffer[String] = ListBuffer()
    for(move <- funcList) {
      for(i <- List.range(1,range(move, s) + 1)){
        if(isLegal(move + i.toString, s))
        //System.err.println("Range given for " + move + ": " + range(move, s))
          moves += move + i.toString
      }
    }
    moves.toList
  }
}

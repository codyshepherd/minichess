import scala.collection.mutable.ListBuffer

/**
  * Created by cody on 4/26/17.
  */
case class Rook(p: Player, var l: Loc) extends Piece(p,l){
  val funcList = List("fwd", "bak", "left", "right")

  def value : Double = 5.0

  def getMe(nl: Loc): Rook = {Rook(p, nl)}

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

  def getMovLoc(m: String): Loc = {
    val nToMov: Int = m.last.toString.toInt
    val mov: String = m.init

    assert(funcList.contains(mov))
    /*
    System.err.println("Rook")
    System.err.println("m: " + m)
    System.err.println("nToMov: " + nToMov)
    System.err.println("mov: " + mov)
    */

    mov match {
      case "fwd" => new Loc(x = this.l.x + this.p.op(nToMov), y = this.l.y)
      case "bak" => new Loc(x = this.l.x - this.p.op(nToMov), y = this.l.y)
      case "left" => new Loc(x = this.l.x, y = this.l.y - nToMov)
      case "right" => new Loc(x = this.l.x, y = this.l.y + nToMov)
    }
  }

  def legalMoves(s: State): List[String] = {
    var moves: ListBuffer[String] = ListBuffer()
    for(move <- funcList) {
      for(i <- List.range(1,range(move, s) + 1)){
        if(isLegal(move + i.toString, s))
          moves += move + i.toString
      }
    }
    moves.toList
  }
}


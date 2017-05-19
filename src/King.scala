/** King.scala
  * minichess
  * Cody Shepherd
  * */

import scala.collection.mutable.ListBuffer

/** This class represents the King piece. It overrides the isLegal function because
  * all its moves are of length 1.
  */
case class King(p: Player, var l: Loc) extends Piece(p,l){
  val funcList = List("fwd1", "bak1", "left1", "right1", "fwdLeft1", "fwdRight1", "bakLeft1", "bakRight1")

  def value : Double = 50.0

  def getMe(nl: Loc): King = {King(p, nl)}

  override def toString : String = {
    p match {
      case Black() => "k"
      case White() => "K"
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case that: King =>
        if (this.p == that.p && this.l == that.l)
          true
        else
          false
      case _ => false
    }
  }

  def getMovLoc(m: String): Loc = {
    assert(funcList.contains(m))

    m match {
      case "fwd1" => new Loc(x = this.l.x + this.p.op(1), y = this.l.y)
      case "bak1" => new Loc(x = this.l.x - this.p.op(1), y = this.l.y)
      case "left1" => new Loc(x = this.l.x, y = this.l.y - 1)
      case "right1" => new Loc(x = this.l.x, y = this.l.y + 1)
      case "fwdLeft1" => new Loc(x = this.l.x + this.p.op(1), y = this.l.y - 1)
      case "fwdRight1" => new Loc(x = this.l.x + this.p.op(1), y = this.l.y + 1)
      case "bakLeft1" => new Loc(x = this.l.x - this.p.op(1), y = this.l.y - 1)
      case "bakRight1" => new Loc(x = this.l.x - this.p.op(1), y = this.l.y + 1)
    }
  }

  override def isLegal(mv: String, s: State): Boolean = {
    if (!funcList.contains(mv)) {
      return false
    }

    val newLoc = getMovLoc(mv)
    if(!isInBounds(newLoc)) {
      return false
    }

    val found = s.pieces.find(p => p.getLoc == newLoc)

    if(found.isDefined){
      if(found.get.getPlayer == this.p) {
        return false
      }
      else {
        return true
      }
    }
    true
  }

  def legalMoves(s: State): List[String] = {
    var moves: ListBuffer[String] = ListBuffer()
    for(move <- funcList){
      if(isLegal(move,s)) {
        moves += move
      }
    }
    moves.toList
  }
}


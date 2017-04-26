/**
  * Created by cody on 4/26/17.
  */
case class Rook(p: Player, l: Loc) extends Piece(p,l){
  val funcs = Map(
    "up" -> PartialFunction(up),
    "down" -> PartialFunction(down),
    "left" -> PartialFunction(left),
    "right" -> PartialFunction(right)
  )
  val funclist:List[String] = funcs.keys.toList

  def up(st: State, num: Int): State = {}
  def down(st: State, num: Int): State = {}
  def left(st: State, num: Int): State = {}
  def right(st: State, num: Int): State = {}

  override def toString : String = {
    p match {
      case Black() => "r"
      case White() => "R"
    }
  }
  override def equals(o: Any): Boolean
  def getMovLoc(m: String): Loc
  def isLegal(mv: String, s: State) : Boolean
}


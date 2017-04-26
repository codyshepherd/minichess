/**
  * Created by cody on 4/26/17.
  */
case class King(p: Player, l: Loc) extends Piece(p,l){

  val funcs: Map[String, State => State]
  val funclist: List[String]
  override def toString : String
  override def equals(o: Any): Boolean
  def getMovLoc(m: String): Loc
  def isLegal(mv: String, s: State) : Boolean
}

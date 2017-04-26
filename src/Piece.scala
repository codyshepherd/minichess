/** Pieces.scala
  * Cody Shepherd
  */

/** Piece is an ADT from which Pawn extends. Having the base class here
  * is a way for me to (hopefully) do less work when it comes time to
  * implement MiniChess, as I can (hopefully) simply extend the other
  * piece types from this class.
  *
  * A piece works by transforming a state with one of its move functions.
  * Its move functions are defined by name in funcList, and are stored in
  * a hash map, whose keys are the values in funcList.
  * */
abstract class Piece(p: Player, l: Loc) {
  val funcs: Map[String, State => State]
  val funclist: List[String]
  override def toString : String
  override def equals(o: Any): Boolean
  def getLoc: Loc = this.l
  def getPlayer: Player = this.p
  def getMovLoc(m: String): Loc
  def isLegal(mv: String, s: State) : Boolean
}
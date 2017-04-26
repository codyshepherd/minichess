/** Pieces.scala
  * Cody Shepherd
  */

/** Loc is just a named pair of x, y coordinates, where x is the row,
  * and y is the column of the board.
  * */
sealed class Loc(val x: Int, val y: Int) {

  override def equals(o: Any): Boolean = {
    o match {
      case that: Loc => {
        if (this.x == that.x && this.y == that.y)
          true
        else
          false
      }
      case _ => false
    }
  }
}

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
  override def toString() : String
  override def equals(o: Any): Boolean
  def getLoc: Loc = this.l
  def getPlayer: Player = this.p
  def getMovLoc(m: String): Loc
  def isLegal(mv: String, s: State) : Boolean
}

/** Pawn is the basic piece, obviously.
  *
  * It can do one of three things under appropriate conditions:
  * move forward, capture to the right, or capture to the left.
  * */
case class Pawn(p: Player, l: Loc) extends Piece(p,l) {

  /** funcs is a hash map of all the movement functions this piece can perform.
    * By convention its members should only be accessed through the items in
    * this piece's funcList. This will allow generating a piece's moves to be
    * at once constrained and programmatic.
    *
    * This member should probably be private?
    * */
  val funcs = Map(
    "fwd" -> PartialFunction(fwd),
    "capRight" -> PartialFunction(capRight),
    "capLeft" -> PartialFunction(capLeft)
    //TODO: Promote
  )

  /** The funcList holds the names of all this piece's movement functions.
    * */
  val funclist:List[String] = funcs.keys.toList

  /** I needed a special kind of equivalence checking
    * */
  override def equals(o:Any): Boolean = {
    o match {
      case that: Pawn => {
        if (this.p == that.p && this.l == that.l)
          true
        else
          false
      }
      case _ => false
    }
  }

  /** Returns the state derived when this piece moves forward, given the argument
    * state.
    *
    * Assumes that forward has already been deemed a legal move.
    *
    * (Should probably figure
    * out a way to handle some possibility where this piece does not exist in the
    * given state)
    * */
  def fwd(st: State): State = {
    val pred = PartialFunction(this.l.equals)
    val np = st.pieces.filterNot((x: Piece) => x == this)

    /*
    System.err.println("Function fwd")
    System.err.println("s.pieces: " + st.pieces)
    System.err.println("np: " + np)
    */

    assert(np != st.pieces)

    this.p match {
      case a: Black => {
        val newp = Pawn(p = this.p, l = new Loc(x = this.l.x-1, y = this.l.y))
        new State(on_move = this.p.opposite, pieces = np :+ newp)
      }
      case a: White => {
        val newp = Pawn(p = this.p, l = new Loc(x = this.l.x+1, y = this.l.y))
        new State(on_move = this.p.opposite, pieces = np :+ newp)
      }
    }
  }

  /** Returns the state derived when this piece captures to the right; i.e. captures, and
    * the captured piece is at a higher column than this piece, irregardless of the color
    * of either piece.
    *
    * Assumes capturing to the right has already been deemed legal.
    * */
  def capRight(st: State): State = {
    val removeMe = (x: Piece) => x == this
    val removeCap = {
      (mp: Piece) => this.getPlayer match {
        case a: Black => mp.getLoc.x == (this.l.x - 1) && mp.getLoc.y == (this.l.y + 1)
        case a: White => mp.getLoc.x == (this.l.x + 1) && mp.getLoc.y == (this.l.y + 1)
      }
    }

    val np = st.pieces.filterNot(removeMe).filterNot(removeCap)
    this.p match {
      case a: Black => {
        val newp = Pawn(p = this.p, l = new Loc(x = this.l.x-1, y = this.l.y+1))
        new State(on_move = this.p.opposite, pieces = np :+ newp)
      }
      case a: White => {
        val newp = Pawn(p = this.p, l = new Loc(x = this.l.x+1, y = this.l.y+1))
        new State(on_move = this.p.opposite, pieces = np :+ newp)
      }
    }
  }

  /** Returns the state derived when this piece captures to the left; i.e. captures,
    * and the captured piece is at a lower column than this piece, irregardless of the
    * color of either piece.
    *
    * Assumes capturig to the left has already been deemed legal.
    * */
  def capLeft(st: State): State = {
    val removeMe = (x: Piece) => x == this
    val removeCap = {
      (mp: Piece) => this.getPlayer match {
        case a: Black => mp.getLoc.x == (this.l.x - 1) && mp.getLoc.y == (this.l.y - 1)
        case a: White => mp.getLoc.x == (this.l.x + 1) && mp.getLoc.y == (this.l.y - 1)
      }
    }
    val np = st.pieces.filterNot(removeMe).filterNot(removeCap)
    this.p match {
      case a: Black => {
        val newp = Pawn(p = this.p, l = new Loc(x = this.l.x-1, y = this.l.y-1))
        new State(on_move = this.p.opposite, pieces = np :+ newp)
      }
      case a: White => {
        val newp = Pawn(p = this.p, l = new Loc(x = this.l.x+1, y = this.l.y-1))
        new State(on_move = this.p.opposite, pieces = np :+ newp)
      }
    }
  }

  /** Returns the location (coordinates) on which this piece would end up if it performed
    * the given move.
    *
    * This method supports checking whether a move is legal.
    * */
  def getMovLoc(m: String): Loc = {
    val newx = this.l.x + this.p.op(1)
    m match {
      case "fwd" => new Loc(x = newx, y = this.l.y)
      case "capRight" => new Loc(x = newx, y = this.l.y + 1)
      case "capLeft" => new Loc(x = newx, y = this.l.y - 1 )
    }
  }

}

case class Rook(p: Player, l: Loc) extends Piece(p,l){}
case class Knight(p: Player, l: Loc) extends Piece(p,l){}
case class Bishop(p: Player, l: Loc) extends Piece(p,l){}
case class Queen(p: Player, l: Loc) extends Piece(p,l){}
case class King(p: Player, l: Loc) extends Piece(p,l){}

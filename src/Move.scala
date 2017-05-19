/** Move.scala
  * minichess
  * Cody Shepherd
  */

/** The Col class is a typed way to discern and pass around columns. It assists in
  * converting between the string representation of the board, as used by the imcs
  * server, and my representation.
  *
  * I'm not convinced it's 100% necessary, but it works.
  * */
sealed abstract class Col {
  override def toString: String
}
case class A() extends Col {
  override def toString: String = "a"
}
case class B() extends Col {
  override def toString: String = "b"
}
case class C() extends Col {
  override def toString: String = "c"
}
case class D() extends Col {
  override def toString: String = "d"
}
case class E() extends Col {
  override def toString: String = "e"
}
case class X() extends Col {
  override def toString: String = "x"
}

/** Same as Col class, but for Rows.
  * */
sealed abstract class Row {
  override def toString: String
}
case class R1() extends Row {
  override def toString: String = "1"
}
case class R2() extends Row {
  override def toString: String = "2"
}
case class R3() extends Row {
  override def toString: String = "3"
}
case class R4() extends Row {
  override def toString: String = "4"
}
case class R5() extends Row {
  override def toString: String = "5"
}
case class R6() extends Row {
  override def toString: String = "6"
}
case class Z() extends Row {
  override def toString: String = "7"
}

/** This class is an abstract representation of a move, and contains all the information
  * needed to know about the move.
  * */
class Move (val p: Piece, val mv: String, val from: (Row, Col), val to: (Row, Col)) {

  override def toString: String = {
    from._2.toString + from._1.toString + "-" + to._2.toString + to._1.toString
  }

  /** Returns the state produced by executing this move on the given state.
    * */
  def go(s: State): State = {
    p.doMove(mv, s)
  }
}

/** A non-move class for cases when a move is required but wouldn't make sense to
  * exist yet. Its go() method returns the state input into it without changing it.
  * */
class Noop (p: Piece = Pawn(White(), new Loc(-1,-1)),
            mv: String = "Noop",
            from: (Row, Col) = (Z(), X()),
            to: (Row, Col) = (Z(), X()))
  extends Move(p, mv,from, to){

  override def go(s: State): State = {
    s
  }
}

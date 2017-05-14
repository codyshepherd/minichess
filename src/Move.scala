/**
  * Created by cody on 4/27/17.
  */
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

class Move (val p: Piece, val mv: String, val from: (Col, Row), val to: (Col, Row)) {

  override def toString: String = {
    from._1.toString + from._2.toString + "-" + to._1.toString + to._2.toString
  }

  def go(s: State): State = {
    p.doMove(mv, s)
  }

}
class Noop (p: Piece = Pawn(White(), new Loc(-1,-1)),
            mv: String = "Noop",
            from: (Col, Row) = (X(), Z()),
            to: (Col, Row) = (X(), Z()))
  extends Move(p, mv,from, to){

  override def go(s: State): State = {
    s
  }
}

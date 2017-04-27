/**
  * Created by cody on 4/27/17.
  */
sealed abstract class Col
case class A() extends Col
case class B() extends Col
case class C() extends Col
case class D() extends Col
case class E() extends Col
case class X() extends Col

sealed abstract class Row
case class R1() extends Row
case class R2() extends Row
case class R3() extends Row
case class R4() extends Row
case class R5() extends Row
case class R6() extends Row
case class Z() extends Row

class Move (from: (Col, Row), to: (Col, Row))
class Noop (from: (Col, Row) = (X(), Z()), to: (Col, Row) = (X(), Z())) extends Move(from, to)

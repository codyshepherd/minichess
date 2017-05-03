/**
  * Created by cody on 4/26/17.
  */

/** Performs (unit) testing on codebase.
  * */
class Tester {
  //read series of states and moves from files
  val g = new Game(AI(White()), AI(Black()))

  val startPieces: List[Piece] = {
    List(
      King(White(), new Loc(0,4)),
      Queen(White(), new Loc(0,3)),
      Bishop(White(), new Loc(0,2)),
      Knight(White(), new Loc(0,1)),
      Rook(White(), new Loc(0,0))
    ) ++
    List(
      King(Black(), new Loc(5,0)),
      Queen(Black(), new Loc(5,1)),
      Bishop(Black(), new Loc(5,2)),
      Knight(Black(), new Loc(5,3)),
      Rook(Black(), new Loc(5,4))
    ) ++
    (for(i <- List.range(0,Params.cols)) yield Pawn(White(), new Loc(1,i))) ++
    (for(i <- List.range(0,Params.cols)) yield Pawn(Black(), new Loc(4,i)))
  }

  val startState = new State(White(), 0, 75.0, 75.0, startPieces)

  def testGame(): Unit = {
    System.err.println("Expected: \n" + startState.toString)
    System.err.println("Actual: \n" + g.s.toString)

    assert(g.s == startState)
  }

  def testPawn(): Unit = {

  }

  def testQueen(): Unit = {

  }

  def testAll(): Unit = {
    testGame()
  }

}

object Testing {
  def main(args: Array[String]): Unit = {
    val t = new Tester()
    t.testAll()
    System.err.println("done")
  }
}
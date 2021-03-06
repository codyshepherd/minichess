/** Tester.scala
  * minichess
  * Cody Shepherd
  */

/** Performs (unit) testing on codebase.
  *
  * Coverage is by no means total.
  * */
class Tester {

  //val path = "/Users/cody/IdeaProjects/minichess/src/teststates/"
  val path = "C:\\Users\\codys\\IdeaProjects\\minichess\\src\\teststates\\"
  //val path = "/u/cls9/IdeaProjects/minichess/src/teststates/"

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
    //System.err.println("Expected: \n" + startState.toString)
    //System.err.println("Actual: \n" + g.s.toString)

    //read series of states and moves from files
    val g = new Game(AI(White(), Params.turnTime), AI(Black(), Params.turnTime))
    assert(g.s == startState)
  }

  def testPawn(): Unit = {
    val w0 = Params.stateFromFile(path ++ "pawnfwd.txt")

    val pawn1 = w0.pieces.filter((p: Piece) => p.getLoc == (1,0) && p.getPlayer==w0.on_move).head
    val b0 = pawn1.doMove("fwd", w0)
    val b0fromfile = Params.stateFromFile(path ++ "pawnfwd2.txt")
    assert(b0 == b0fromfile)
    val bp = b0.pieces.filter((pe: Piece) => pe.getLoc==(4,1) && pe.getPlayer==b0.on_move).head
    val w1 = bp.doMove("fwd", b0)
    val w1fromfile = Params.stateFromFile(path ++ "pawncaprightwhite.txt")
    assert(w1 == w1fromfile)
    val wp = w1.pieces.filter((pe: Piece) => pe.getLoc==(2,0) && pe.getPlayer==w1.on_move).head
    val b1 = wp.doMove("capRight", w1)
    val b1fromfile = Params.stateFromFile(path ++ "pawncaprightwhite2.txt")
    assert(b1 == b1fromfile)

    val crstart = Params.stateFromFile(path ++ "pawncaprightblack.txt")
    val blackpawn = crstart.pieces.filter((p: Piece) => p.getLoc == (3,1) && p.getPlayer == crstart.on_move).head
    val crafter = blackpawn.doMove("capRight", crstart)
    val crafterfromfile = Params.stateFromFile(path ++ "pawncaprightblack2.txt")
    assert(crafter == crafterfromfile)

    val bstart = Params.stateFromFile(path ++ "pawncapleftblack.txt")
    val bpawn = bstart.pieces.filter((pe: Piece) => pe.getLoc==(3,1) && pe.getPlayer==bstart.on_move).head
    val bafter = bpawn.doMove("capLeft", bstart)
    val bafterfromfile = Params.stateFromFile(path ++ "pawncapleftblack2.txt")
    assert(bafter == bafterfromfile)

    val wstart = Params.stateFromFile(path ++ "pawncapleftwhite.txt")
    val wpawn = wstart.pieces.filter((pe: Piece) => pe.getLoc==(2,2) && pe.getPlayer==wstart.on_move).head
    val wafter = wpawn.doMove("capLeft", wstart)
    val wafterfromfile = Params.stateFromFile(path ++ "pawncapleftwhite2.txt")
    assert(wafter == wafterfromfile)

    System.err.println("White Pawn moves from start: ")
    System.err.println(w0.pieces.find(p => p.getPlayer == White() && p.getLoc == (1,1)).get.legalMoves(w0))
  }

  def testQueen(): Unit = {
    val w0 = Params.stateFromFile(path ++ "queentest.txt")

    val q1 = w0.pieces.filter((p: Piece) => p.getLoc == (5,1) && p.getPlayer == Black()).head

    assert(!q1.legalMoves(w0).contains("fwd5"))

    val w1 = Params.stringsToState("1 B\n.....\n.....\n..Q..\n.....\n.....\n.....".split("\n").toList)
    System.err.println(w1.pieces.find(p => p.isInstanceOf[Queen]).get.legalMoves(w1))
  }

  def testKing(): Unit = {
    val w0 = Params.stateFromFile(path ++ "pawnfwd.txt")

    val k1 = w0.pieces.filter((p: Piece) => p.getLoc == (0,4) && p.getPlayer==w0.on_move).head

    val mvs = k1.legalMoves(w0)
    assert(mvs.isEmpty)

    assert(w0.pieces.exists((p: Piece) => p.isInstanceOf[King]))
    assert(!w0.pieces.filterNot((p: Piece) => p.isInstanceOf[King]).exists((p: Piece) => p.isInstanceOf[King]))
  }

  def testKnight(): Unit = {
    val w0 = Params.stateFromFile(path ++ "pawnfwd.txt")

    val n1 = w0.pieces.filter((p: Piece) => p.isInstanceOf[Knight] && p.getPlayer==w0.on_move).head

    val mvs = n1.legalMoves(w0)
    System.err.println(mvs)
    assert(mvs.toSet == Set("longLeft1", "longRight1"))

  }

  def testState(): Unit = {
    val w0 = Params.stateFromFile(path ++ "pawnfwd.txt")
    val w1 = Params.stateFromFile(path ++ "testmoves.txt")

    assert(w0.value == 0)

    for (move <- w1.legalMoves){
      System.err.println(move.p)
      System.err.println(move)
      System.err.println(move.go(w1))
      System.err.println(move.go(w1).b_value)
      System.err.println(move.go(w1).w_value)
      System.err.println(move.go(w1).value)
    }
  }

  def testAll(): Unit = {
    testGame()
    testPawn()
    testKnight()
    testKing()
    testQueen()
    testState()
  }



}

object Testing {
  def main(args: Array[String]): Unit = {
    val t = new Tester()
    t.testAll()
    System.err.println("All tests passed.")
  }
}
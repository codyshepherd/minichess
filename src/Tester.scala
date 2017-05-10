/**
  * Created by cody on 4/26/17.
  */

/** Performs (unit) testing on codebase.
  * */
class Tester {

  val path = "/Users/cody/IdeaProjects/minichess/src/teststates/"

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
    val g = new Game(AI(White()), AI(Black()))
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
  }

  def testQueen(): Unit = {

  }

  def testKing(): Unit = {
    val w0 = Params.stateFromFile(path ++ "pawnfwd.txt")

    val k1 = w0.pieces.filter((p: Piece) => p.getLoc == (0,4) && p.getPlayer==w0.on_move).head

    val mvs = k1.legalMoves(w0)
    assert(mvs.isEmpty)
  }

  def testKnight(): Unit = {
    val w0 = Params.stateFromFile(path ++ "pawnfwd.txt")

    val n1 = w0.pieces.filter((p: Piece) => p.getLoc == (0,1) && p.getPlayer==w0.on_move).head

    val mvs = n1.legalMoves(w0)
    assert(mvs == List("longLeft1", "longRight1", "shortLeft2", "shortRight4"))

  }

  def testAll(): Unit = {
    testGame()
    testPawn()
    testKnight()
    testKing()
  }

}

object Testing {
  def main(args: Array[String]): Unit = {
    val t = new Tester()
    t.testAll()
    System.err.println("All tests passed.")
  }
}
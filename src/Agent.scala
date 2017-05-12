
/**
  * Created by cody on 4/26/17.
  */

import java.time.LocalTime
import java.time.ZoneId

/** This class represents the AI Player that makes moves and plays the game.
  * */
sealed abstract class Agent(p: Player) {

  //Keep track of board piece value as you go -- state.value?

  //heuristics for move ordering:
    // Material Value
    // Queen capture
    // Control the center
    // Pawn formation
    // Pawn advancement / close to promotion

  // dynamically increase search depth over time
    // use remembered value from current depth -1
    // use with ttable to amortize work done
    // "Iterative deepening"

  // Thinking on opponent's time

  val colMap: Map[Char, Col] = Map[Char, Col](
    'a' -> A(),
    'b' -> B(),
    'c' -> C(),
    'd' -> D(),
    'e' -> E()
  ).withDefaultValue(X())

  val rowMap: Map[Char, Row] = Map[Char, Row](
    '1' -> R1(),
    '2' -> R2(),
    '3' -> R3(),
    '4' -> R4(),
    '5' -> R5(),
    '6' -> R6()
  ).withDefaultValue(Z())

  def stringToLoc(s: String): Loc = {
    if(s.length != 2)
      return new Loc(-1,-1)

    val row = s.last match {
      case '1' => 0
      case '2' => 1
      case '3' => 2
      case '4' => 3
      case '5' => 4
      case '6' => 5
    }

    val col = s.head match {
      case 'a' => 0
      case 'b' => 1
      case 'c' => 2
      case 'd' => 3
      case 'e' => 4
    }
    new Loc(row, col)
  }

  def move(s: State): String
}

case class AI(p: Player) extends Agent(p) {

  def stringToMove(p: Piece, s: String): Move = {
    val newLoc = p.getMovLoc(s)
    val fromRow: Row = p.getLoc.x match {
      case 0 => R1()
      case 1 => R2()
      case 2 => R3()
      case 3 => R4()
      case 4 => R5()
      case 5 => R6()
      case _ => Z()
    }
    val fromCol: Col = p.getLoc.y match {
      case 0 => A()
      case 1 => B()
      case 2 => C()
      case 3 => D()
      case 4 => E()
      case _ => X()
    }
    val toRow: Row = newLoc.x match {
      case 0 => R1()
      case 1 => R2()
      case 2 => R3()
      case 3 => R4()
      case 4 => R5()
      case 5 => R6()
      case _ => Z()
    }
    val toCol: Col = newLoc.y match {
      case 0 => A()
      case 1 => B()
      case 2 => C()
      case 3 => D()
      case 4 => E()
      case _ => X()
    }
    new Move(p, s, (fromCol, fromRow), (toCol, toRow))
  }

  def getLegalMoves(s: State): List[Move] = {
    //System.err.println("getLegalMoves s argument: " + s.toString)

    val mypieces = s.pieces.filter((p:Piece) => p.getPlayer == this.p)

    //System.err.println("getLegalMoves mypieces: " + mypieces.toString())

    var moves: List[Move] = List()

    for (piece <- mypieces){
      val pieceMoves = piece.legalMoves(s)
      for(move <- pieceMoves){
        moves = moves :+ stringToMove(piece, move)
      }
    }
    moves.distinct
  }

  def heuristicSort(mvs: List[Move], s: State): List[Move] = {
    val zipped = mvs zip (for (move <- mvs) yield move.go(s))

    s.on_move match {
      case White() => (for (tup <- scala.util.Sorting.stableSort(zipped, (e1: (Move, State), e2: (Move, State)) => e1._2.w_value > e2._2.w_value)) yield tup._1).toList
      case Black() => (for (tup <- scala.util.Sorting.stableSort(zipped, (e1: (Move, State), e2: (Move, State)) => e1._2.b_value > e2._2.b_value)) yield tup._1).toList
    }

  }

  def isWin(s: State): Boolean = {
    val whitePieces = s.pieces.filter((p: Piece) => p.getPlayer == White())
    val blackPieces = s.pieces.filter((p: Piece) => p.getPlayer == Black())

    var ret1 = false
    var ret2 = false
    for(w <- whitePieces){
      if (w.toString == "K")
        ret1 = true
      //System.err.println(w.toString)
    }
    for(b <- blackPieces){
      if(b.toString == "k")
        ret2 = true
      //System.err.println(b.toString)
    }

    !(ret1 && ret2)
  }

  //TODO: Iterative deepening -- get best move at ply 1, get best move at ply 2, etc, until out of time, use move
          // from last full ply searched

  def alphaBeta(s: State, depth: Int, alpha: Double, beta: Double, player: Player): Double = {
    //System.err.println("alpha-beta depth: " + depth)
    if (depth <= 0) {
      //System.err.println("hit depth 0. White: " + s.w_value + " Black: " + s.b_value)
      //val sHash = Params.zobristHash(s)
      player match {
        case White() =>{
          //Params.ttable = Params.ttable + (sHash -> new Tpos(s,s.w_value, Params.plyDepth, Exact()))
          return s.w_value
        }
        case Black() => {
          //Params.ttable = Params.ttable + (sHash -> new Tpos(s,s.b_value, Params.plyDepth, Exact()))
          return s.b_value
        }
      }
    }

    if (isWin(s)) {
      //System.err.println("found winning state. White: " + s.w_value + " Black: " + s.b_value)
      //val sHash = Params.zobristHash(s)
      //Params.ttable = Params.ttable + (sHash -> s)
      player match {
        case White() => return s.w_value
        case Black() => return s.b_value
      }
    }

    //xxx: check ttable
    var sHash: Long = 0

    var bestValue: Double = Double.NegativeInfinity
    var tempAlpha = alpha
    var tempBeta = beta
    val playerPieces = s.pieces.filter((p: Piece) => p.getPlayer == s.on_move)
    val moves = (for (piece <- playerPieces) yield for (move <- piece.legalMoves(s)) yield piece.doMove(move, s)).flatten

    var v: Double = 0.0
    var bestMove: State = moves.head

    for (move <- moves) {
      sHash = Params.zobristHash(move, Params.plyDepth-depth)
      val tPos = Params.ttable get sHash
      if(tPos.isDefined){
        if(tPos.get.t == Exact())
          return tPos.get.score
        else if (tPos.get.t == Lower())
          tempAlpha = math.max(tempAlpha, tPos.get.score)
        else if (tPos.get.t == Upper())
          tempBeta = math.min(tempBeta, tPos.get.score)
        if(tempAlpha >= tempBeta)
          return tPos.get.score
      }

      v = -alphaBeta(move, depth - 1, -tempBeta, -tempAlpha, player.opposite)
      if (v > bestValue)
        bestMove = move
      bestValue = math.max(bestValue, v)
      tempAlpha = math.max(tempAlpha, v)
      if (tempAlpha >= tempBeta)
        return bestValue // beta cut-off
    }

    var ttFlag: NodeType = Exact()
    if(bestValue <= alpha)
      ttFlag = Upper()
    else if (bestValue >= beta)
      ttFlag = Lower()
    else
      ttFlag = Exact()
    Params.ttable = Params.ttable + (sHash -> new Tpos(bestMove, bestValue, Params.plyDepth-depth, ttFlag))

    bestValue
  }

  def move(s: State): String = {

    val startTime = LocalTime.now(ZoneId.systemDefault()).toSecondOfDay

    if (s.on_move != this.p){
      System.err.println("Not our turn, generating noop")
      //xxx: think on opponent's time
      new Noop().toString
    }
    else{
      val sortedMoves = heuristicSort(scala.util.Random.shuffle(getLegalMoves(s)), s)
      //System.err.println("Number of legal moves: " + sortedMoves.length)

      var bestMove: Move = new Noop()
      var bestMoveVal: Double = Double.NegativeInfinity
      var moveVal: Double = 0.0

      for (move <- sortedMoves){
        //System.err.println("Move being considered: " + move)
        moveVal = -alphaBeta(move.go(s), Params.plyDepth, Double.NegativeInfinity, Double.PositiveInfinity, s.on_move.opposite)
        //System.err.println("value of that move: " + moveVal)
        if (moveVal > bestMoveVal){
          bestMoveVal = moveVal
          bestMove = move
        }
        if (LocalTime.now(ZoneId.systemDefault()).toSecondOfDay - startTime > Params.turnTime)
          return bestMove.toString
      }
      //System.err.println("AI move returning: " + bestMove)
      bestMove.toString
    }
  }

}

/*
case class Human (p: Player) extends Agent(p) {

  def move(s: State): Move = {
    var legalMove = false
    while(!legalMove) {
      val line = scala.io.StdIn.readLine()
      if (line.length == 5){
        val mvstring = line.split("-")
        val from = stringToLoc(mvstring.head)

        val pieceToMove = s.pieces.find((p: Piece) => p.getLoc == from)

        val to = stringToLoc(mvstring.last)

        val move: String = pieceToMove match {
          case Some(a) => {
            var temp: String = ""
            a.getPlayer match {
              case Black() => {
                if(to.x < from.x)
                  temp = temp + "fwd"
                else if(to.x > from.x)
                  temp = temp + "bak"
              }
              case White() =>
            }
          }
          case None =>
        }
      }
    }
    new Noop()
  }

  def stringToMove(mv: String, s: State): Move = {
    new Move(pieceToMove, (colMap(from.head), rowMap(from.last)), (colMap(to.head), rowMap(to.last)))
  }

}
*/
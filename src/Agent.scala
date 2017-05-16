
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



  def heuristicSort(mvs: List[Move], s: State): List[Move] = {
    val zipped = mvs zip (for (move <- mvs) yield move.go(s))

    var list: List[Move] = List()

    s.on_move match {
      case White() => {
        list = (for (tup <- scala.util.Sorting.stableSort(zipped, (e1: (Move, State), e2: (Move, State)) => e1._2.value < e2._2.value)) yield tup._1).toList
        /*
        val altlist = list
        for(move <- altlist){
          if(//!move.go(s).pieces.contains((p: Piece) => p.isInstanceOf[King] && p.getPlayer == Black())
          //|| !move.go(s).pieces.contains((p: Piece) => p.isInstanceOf[Queen] && p.getPlayer == Black())
          /*||*/ move.p.isInstanceOf[King]
          /*|| move.p.isInstanceOf[Queen]*/)
            list = move :: list.filterNot((m: Move) => m == move)
          else if (move.p.getLoc == (1,3))
            list = list.filterNot((m: Move) => m == move) :+ move
        }
        */
        list
      }
      case Black() => {
        list = (for (tup <- scala.util.Sorting.stableSort(zipped, (e1: (Move, State), e2: (Move, State)) => e1._2.value < e2._2.value)) yield tup._1).toList

        /*
        val altlist = list
        for(move <- altlist){
          if(//!isLoss(move.go(s))
            //|| !move.go(s).pieces.contains((p: Piece) => p.isInstanceOf[Queen] && p.getPlayer == White())
            /*||*/ move.p.isInstanceOf[King]
            /*|| move.p.isInstanceOf[Queen]*/)
            list = move :: list.filterNot((m: Move) => m == move)
          else if (move.p.getLoc == (4,1))
            list = list.filterNot((m: Move) => m == move) :+ move
        }
        */
        list
      }
    }


  }

  def isWin(s: State): Boolean = {
    if(s.on_move == White()){
      val blackPieces = s.pieces.filter((p: Piece) => p.getPlayer == Black())
      if(!blackPieces.exists((p: Piece) => p.isInstanceOf[King]))
        true
      else false
    }
    else{
      val whitePieces = s.pieces.filter((p: Piece) => p.getPlayer == White())
      if(!whitePieces.exists((p: Piece) => p.isInstanceOf[King]))
        true
      else false
    }
  }

  def isLoss(s: State): Boolean = {
     if(s.on_move == Black()){
      val blackPieces = s.pieces.filter((p: Piece) => p.getPlayer == Black())
      if(!blackPieces.exists((p: Piece) => p.isInstanceOf[King]))
        true
      else false
    }
    else{
      val whitePieces = s.pieces.filter((p: Piece) => p.getPlayer == White())
      if(!whitePieces.exists((p: Piece) => p.isInstanceOf[King]))
        true
      else false
    }
  }

  def alphaBeta(s: State, depth: Int, alpha: Double, beta: Double): Double = {
    if (isWin(s)) {
      return Double.PositiveInfinity
      //return s.value
    }

    if (isLoss(s)) {
      return Double.NegativeInfinity
      //return s.value
    }

    if (depth <= 0) {
      return s.value
    }

    var bestScore = Double.NegativeInfinity

    val movesList: List[Move] = heuristicSort(s.legalMoves,s)
    var tempAlpha = alpha

    for(move <- movesList){
      val score = -alphaBeta(move.go(s),depth - 1, -beta, -tempAlpha)
      if(score >= beta)
        return score
      if(score > bestScore){
        bestScore = score
        if(score > tempAlpha)
          tempAlpha = score
      }
    }

    bestScore
  }

  def store(sHash: Long, depth: Int, bestValue: Double, alpha: Double, beta: Double): Double = {

    var ttFlag: NodeType = Exact()
    if (bestValue <= alpha)
      ttFlag = Upper()
    else if (bestValue >= beta)
      ttFlag = Lower()
    else
      ttFlag = Exact()
    Params.ttable += (sHash -> new Tpos(bestValue, depth, ttFlag))

    bestValue
  }

  /*
  def alphaBeta(s: State, depth: Int, alpha: Double, beta: Double): Double = {

    //val playerPieces = s.pieces.filter((p: Piece) => p.getPlayer == s.on_move)
    //val moves = (for (piece <- playerPieces) yield for (move <- piece.legalMoves(s)) yield piece.doMove(move, s)).flatten
    val movesList: List[Move] = heuristicSort(s.legalMoves,s)
    val moves: List[State] = for (mv <- movesList) yield mv.p.doMove(mv.mv,s)


    if (isWin(s)) {
      //return Double.PositiveInfinity
      return s.value
    }

    if(isLoss(s)) {
      //return Double.NegativeInfinity
      return s.value
    }

    if (depth <= 0) {
      return s.value
    }

    var tempAlpha = alpha
    var tempBeta = beta

    var sHash: Long = Params.zobristHash(s, depth)

    if(Params.isTtableOn) {
      val tPos = Params.ttable get sHash
      if (tPos.isDefined && tPos.get.depth >= depth) {
        if (tPos.get.t == Exact())
          return tPos.get.score
        else if (tPos.get.t == Lower())
          tempAlpha = math.max(tempAlpha, tPos.get.score)
        else if (tPos.get.t == Upper())
          tempBeta = math.min(tempBeta, tPos.get.score)
        if (tempAlpha >= tempBeta)
          return tPos.get.score
      }
    }

    var bestValue: Double = Double.NegativeInfinity
    var v: Double = 0.0
    var bestMove: State = moves.head

    for (move <- moves) {

      v = -alphaBeta(move, depth - 1, -/*beta*/tempBeta, -tempAlpha)
      if (v > bestValue)
        if (v > bestValue)
        bestMove = move
      bestValue = math.max(bestValue, v)
      tempAlpha = math.max(tempAlpha, v)

      if (tempAlpha >= tempBeta || LocalTime.now(ZoneId.systemDefault()).toSecondOfDay - Params.startTime > Params.turnTime + s.moveNum/5) {
        if(Params.isTtableOn) {
          var ttFlag: NodeType = Exact()
          if (bestValue <= alpha)
            ttFlag = Upper()
          else if (bestValue >= tempBeta)
            ttFlag = Lower()
          else
            ttFlag = Exact()
          Params.ttable += (sHash -> new Tpos(bestMove, bestValue, depth, ttFlag))
        }
        return bestValue // beta cut-off
      }
    }

    if(Params.isTtableOn) {
      var ttFlag: NodeType = Exact()
      if (bestValue <= alpha)
        ttFlag = Upper()
      else if (bestValue >= tempBeta)
        ttFlag = Lower()
      else
        ttFlag = Exact()
      Params.ttable += (sHash -> new Tpos(bestMove, bestValue, depth, ttFlag))
    }

    bestValue
  }
  */

  def move(s: State): String = {
    var counter = 0
    Params.startTime = LocalTime.now(ZoneId.systemDefault()).toSecondOfDay

    if (s.on_move != this.p){
      System.err.println("Not our turn, generating noop")
      //xxx: think on opponent's time
      new Noop().toString
    }
    else {
      val sortedMoves = heuristicSort(s.legalMoves, s)
      //val sortedMoves = heuristicSort(s.legalMoves, s)

      Params.cachedBestMove = sortedMoves.head
      Params.cachedBestMoveVal = Double.NegativeInfinity
      var bestMove: Move = Params.cachedBestMove
      var bestMoveVal: Double = Double.NegativeInfinity
      var moveVal: Double = 0.0

      for (d <- List.range(1, Params.plyDepth+1)) {

        for (move <- sortedMoves) {
          counter += 1
          moveVal = -alphaBeta(move.go(s), d, Double.NegativeInfinity, Double.PositiveInfinity)
          if (moveVal > bestMoveVal) {
            bestMoveVal = moveVal
            bestMove = move
          }
          if (LocalTime.now(ZoneId.systemDefault()).toSecondOfDay - Params.startTime > Params.turnTime + s.moveNum/5) {
            System.err.println("Returning move from depth " + (d - 1))
            return Params.cachedBestMove.toString
          }
        }
        if(bestMoveVal > Params.cachedBestMoveVal) {
          Params.cachedBestMoveVal = bestMoveVal
          Params.cachedBestMove = bestMove
        }
        counter = 0
      }
      System.err.println("Returning move from depth " + Params.plyDepth)
      Params.cachedBestMove.toString
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
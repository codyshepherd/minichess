
/**
  * Created by cody on 4/26/17.
  */

import java.time.LocalTime
import java.time.ZoneId

/** This class represents the AI Player that makes moves and plays the game.
  * */
sealed abstract class Agent(p: Player, t: Int) {

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
    if(s.length != 2) {
      return new Loc(-1, -1)
    }

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

case class AI(p: Player, t: Int) extends Agent(p, t) {

  def heuristicSort(mvs: List[Move], s: State): List[Move] = {
    mvs.sortBy(st => st.go(s).value)
  }

  def alphaBeta(s: State, depth: Int, alpha: Double, beta: Double): Double = {
    if(depth <= 0 || s.pieces.count(p => p.isInstanceOf[King]) < 2)
      return s.value

    val sortedMoves = heuristicSort(s.legalMoves, s)
    //val sortedMoves = scala.util.Random.shuffle(s.legalMoves)
    val firstMove = sortedMoves.headOption

    var sHash: Long = Params.zobristHash(s, depth)

    if(Params.isTtableOn) {
      var hashAlpha = alpha
      var hashBeta = beta
      val tPos = Params.ttable get sHash
      if (tPos.isDefined && tPos.get.depth >= depth) {
        if (tPos.get.t == Exact())
          return tPos.get.score
        else if (tPos.get.t == Lower())
          hashAlpha = math.max(hashAlpha, tPos.get.score)
        else if (tPos.get.t == Upper())
          hashBeta = math.min(hashBeta, tPos.get.score)
        if (hashAlpha >= hashBeta)
          return tPos.get.score
      }
    }


    var sprime = firstMove.get.go(s)
    var vprime = -alphaBeta(sprime, depth - 1, -beta, -alpha)

    if(vprime > beta) {
      if(Params.isTtableOn){
        return store(sHash,depth,vprime,alpha,beta)
      }
      else {
        return vprime
      }
    }

    var tempAlpha = math.max(alpha, vprime)

    for (nextMove <- sortedMoves.tail){
      sprime = nextMove.go(s)
      val v = -alphaBeta(sprime, depth - 1, -beta, -tempAlpha)
      if(v >= beta){
        if(Params.isTtableOn) {
          return store(sHash, depth, v, alpha, beta)
        }
        else {
          return v
        }
      }
      vprime = math.max(v, vprime)
      tempAlpha = math.max(tempAlpha, v)

    }
    if(Params.isTtableOn) {
      store(sHash, depth, vprime, alpha, beta)
    }
    else {
      vprime
    }
  }

  def store(sHash: Long, depth: Int, bestValue: Double, alpha: Double, beta: Double): Double = {

    var ttFlag: NodeType = Exact()
    if (bestValue <= alpha) {
      ttFlag = Upper()
    }
    else if (bestValue >= beta) {
      ttFlag = Lower()
    }
    else {
      ttFlag = Exact()
    }

    Params.ttable.update(sHash, new Tpos(bestValue, depth, ttFlag))

    bestValue
  }

  def move(s: State): String = {
    Params.startTime = LocalTime.now(ZoneId.systemDefault()).toSecondOfDay

    if (s.on_move != this.p){
      System.err.println("Not our turn, generating noop")
      new Noop().toString
    }
    else {
      val sortedMoves = heuristicSort(s.legalMoves, s)

      Params.cachedBestMove = sortedMoves.head
      Params.cachedBestMoveVal = Double.NegativeInfinity
      var bestMove: Move = Params.cachedBestMove
      var bestMoveVal: Double = Double.NegativeInfinity
      var moveVal: Double = 0.0

      for (d <- List.range(1, Params.plyDepth+1)) {

        for (move <- sortedMoves) {
          moveVal = -alphaBeta(move.go(s), d, Double.NegativeInfinity, Double.PositiveInfinity)
          if (moveVal > bestMoveVal) {
            bestMoveVal = moveVal
            bestMove = move
          }
          if (LocalTime.now(ZoneId.systemDefault()).toSecondOfDay - Params.startTime > t) {
            System.err.println("Returning move from depth " + (d - 1))
            return Params.cachedBestMove.toString
          }
        }
        if(bestMoveVal > Params.cachedBestMoveVal) {
          Params.cachedBestMoveVal = bestMoveVal
          Params.cachedBestMove = bestMove
        }
        /*
        val results: List[(Double, Move)] = sortedMoves.par.map(m => (-alphaBeta(m.go(s), d, Double.NegativeInfinity, Double.PositiveInfinity), m)).toList
        val best: (Double, Move) = results.maxBy(_._1)
        if(best._1 > Params.cachedBestMoveVal) {
          Params.cachedBestMoveVal = best._1
          Params.cachedBestMove = best._2
        }
        if (LocalTime.now(ZoneId.systemDefault()).toSecondOfDay - Params.startTime > Params.turnTime) {
          System.err.println("Returning move from depth " + (d - 1))
          return Params.cachedBestMove.toString
        }
        */
      }
      System.err.println("Returning move from max depth " + Params.plyDepth)
      Params.cachedBestMove.toString
    }
  }

}

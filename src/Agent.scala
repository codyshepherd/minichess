/** Agent.scala
  * minichess
  * Cody Shepherd
  */

import java.time.LocalTime
import java.time.ZoneId

/** Agent is a base class that allows extension to either an AI player (currently implemented
  * below) and, in the future, a human player.
  * */
sealed abstract class Agent(p: Player, t: Int) {

  /** The following two maps allow fast and easy mapping of a read-in character to
    * the appropriate row or column.*/
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

  /** This method decodes one part of a move string like those used by the IMCS server
    * and turns it into a location on the board.
    * */
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

  /** All Agents must implement a move function in order for the game to work.
    * */
  def move(s: State): String
}

/** The AI class represents, surprise, an automated AI Player.
  * */
case class AI(p: Player, t: Int) extends Agent(p, t) {

  /** Returns a list of moves sorted ascending by material value.
    *
    * This method is defined as a function to allow me to drop in a different heuristic
    * at some later time. Currently it sorts by material value.
    * */
 def heuristicSort(mvs: List[Move], s: State): List[Move] = {
    mvs.sortBy(st => st.go(s).value)
  }

  /** Returns the best-found value of the given state at the given depth.
    *
    * The negamax monte-carlo tree search with alpha-beta pruning and use of a
    * transposition table.
    * */
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

  /** Returns the second (bestValue) argument.
    *
    * Stores a value in the transposition table, along with pertinent information (whether
    * the value represents an upper or lower bound or an exact value, and the depth of search
    * the value represents).
    * */
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

  /** Returns the best move from the current state as a "x1-y2" string, based on
    * the results of negamax monte-carlo tree search with a-b pruning.
    *
    * Uses iterative deepening to search as deep as possible based on time constraints.
    * */
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

      }
      System.err.println("Returning move from max depth " + Params.plyDepth)
      Params.cachedBestMove.toString
    }
  }

}

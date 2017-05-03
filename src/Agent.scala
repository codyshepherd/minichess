
/**
  * Created by cody on 4/26/17.
  */

/** This class represents the AI Player that makes moves and plays the game.
  * */
sealed abstract class Agent(p: Player) {
  //responsible for checking that a prospective move is legal

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

  def move(s: State): String
}

case class AI(p: Player) extends Agent(p) {



  /*
  def isLegal(piece: Piece, mv: String, s: State): Boolean = {
    val newLoc = piece.getMovLoc(mv)

    if(newLoc == piece.getLoc)
      return true

    if(!isInBounds(newLoc))
      return false

    val thoseAtLoc = s.pieces.filter((p: Piece) => p.getLoc == newLoc)
    assert(thoseAtLoc.length <= 1)
    val atLoc = thoseAtLoc.head

    atLoc match {}


  }
  */

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
    val mypieces = s.pieces.filter((p:Piece) => p.getPlayer == this.p)

    var moves: List[Move] = List()

    for (piece <- mypieces){
      val pieceMoves = piece.legalMoves(s)
      for(move <- pieceMoves){
        moves = moves :+ stringToMove(piece, move)
      }
    }
    moves
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

    if (!whitePieces.contains((p: Piece) => p.isInstanceOf[King]) || !blackPieces.contains((p: Piece) => p.isInstanceOf[King]))
      true
    else
      false
  }

  def alphaBeta(s: State, depth: Int, alpha: Double, beta: Double, player: Player): Double = {
    if (depth == 0 || isWin(s)){
      player match {
        case White() => s.w_value
        case Black() => s.b_value
      }
    }

    if(player == s.on_move){
      var v: Double = Double.NegativeInfinity
      val playerPieces = s.pieces.filter((p: Piece) => p.getPlayer == s.on_move)
      val moves = (for(piece <- playerPieces) yield for (move <- piece.legalMoves(s)) yield piece.doMove(move, s)).flatten

      for (move <- moves){
        v = math.max(v, alphaBeta(move, depth - 1, alpha, beta, player))
        val tempAlpha = math.max(alpha, v)
        if (beta <= tempAlpha)
          return v  // beta cut-off
      }
      v
    }
    else {
      var v: Double = Double.PositiveInfinity
      val playerPieces = s.pieces.filter((p: Piece) => p.getPlayer == s.on_move)
      val moves = (for(piece <- playerPieces) yield for (move <- piece.legalMoves(s)) yield piece.doMove(move, s)).flatten

      for (move <- moves){
        v = math.min(v, alphaBeta(move, depth - 1, alpha, beta, player))
        val tempBeta = math.min(alpha, v)
        if (tempBeta <= alpha)
          return v  // beta cut-off
      }
      v
    }

  }

  def move(s: State): String = {

    //xxx: check ttable

    if (s.on_move != this.p){
      //xxx: think on opponent's time
      new Noop().toString
    }
    else{
      val sortedMoves = heuristicSort(getLegalMoves(s), s)

      var bestMove: Move = new Noop()
      var bestMoveVal: Double = 0.0
      var moveVal: Double = 0.0

      for (move <- sortedMoves){
        moveVal = alphaBeta(move.go(s), Params.plyDepth, Double.NegativeInfinity, Double.PositiveInfinity, s.on_move)
        if (moveVal > bestMoveVal){
          bestMoveVal = moveVal
          bestMove = move
        }
      }
      bestMove.toString
    }
  }

}
/*
case class Human (p: Player) extends Agent(p) {

  def move(s: State): Move = {
    while(true) {
      val line = scala.io.StdIn.readLine()
      val mv = stringToMove(line)
      mv match {
        case Some(a) => return a
        case None =>
      }
    }
    new Noop()
  }

  def stringToMove(s: String): Option[Move] = {
    if(s.length != 5)
      return None

    val mvlist = s.split(" ")
    val from = mvlist.head
    val to = mvlist.last

    Option(new Move((colMap(from.head), rowMap(from.last)), (colMap(to.head), rowMap(to.last))))
  }

}
*/
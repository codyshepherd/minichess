/** State.scala
  * minichess
  * Cody Shepherd
  * */
import scala.collection.mutable.ListBuffer


/** This class represents a board state or position at some time t.
  * */
class State(val on_move: Player, val moveNum: Int, var b_value: Double = 0, var w_value: Double = 0, val pieces: List[Piece]){

  /** The list of all legal moves the player on move can make within this state.
    * */
  val legalMoves: List[Move] = {
    val mypieces = pieces.filter(p => p.getPlayer == on_move)

    var moves: scala.collection.mutable.ListBuffer[Move] = ListBuffer()

    for (piece <- mypieces){
      val pieceMoves = piece.legalMoves(this)
      for(move <- pieceMoves){
        moves += Params.stringToMove(piece, move)
      }
    }
    moves.distinct.toList
  }

  val value: Double = heuristicValue

  override def toString: String = {
    var str = ""

    var tempstr = ""
    for (r <- List.range(0, Params.rows)){
      for (c <- List.range(0, Params.cols)) {
        val pc = pieces.find((x:Piece) => if (x.getLoc.x == r && x.getLoc.y == c) true else false)
        pc match {
          case Some(a) => tempstr += a.toString
          case _ => tempstr += "."
        }
      }
      str = tempstr + '\n' + str
      tempstr = ""
    }

    if(on_move == White())
      str = moveNum.toString + " W\n" + str
    else
      str = moveNum.toString + " B\n" + str

    str

  }

  /** Returns the "heuristic value" of the state (from the on move-side's POV).
    *
    * The heuristic used here can be chosen from the terminal (material value, or
    * material value + mobility)
    * */
  def heuristicValue: Double = {
    if(Params.mobility) {
      val l = this.legalMoves.length.toDouble
      if (on_move == White())
        (w_value - b_value) + l
        //(w_value - b_value) * Params.mvWeight + l * Params.mbWeight
      else
        (b_value - w_value) + l
        //(b_value - w_value) * Params.mvWeight + l * Params.mbWeight
    }
    else{
      if(on_move == White())
        w_value - b_value
      else
        b_value - w_value
    }
  }

  /** The purpose of this method is probably obvious.
    *
    * However it is worth specifying, two states are equal if:
    *   - They have the same player on move
    *   - Their value is the same
    *   - They contain pieces at the same locations of the same color, in any order.
    *     - i.e. the pieces do not have to be the same memory references, just have the same
    *       attributes.
    * */
  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case s: State => {
        if (this.on_move != s.on_move) {
          return false
        }
        if (this.b_value != s.b_value || this.w_value != s.w_value) {
          return false
        }

        if (this.pieces.length != s.pieces.length)
          return false

        for (tp <- this.pieces) {
          val j = s.pieces.find((x: Piece) => x == tp)
          j match {
            case Some(sp) =>
            case _ => return false
          }
        }
        true
      }
      case _ => {
        false
      }
    }
  }
}

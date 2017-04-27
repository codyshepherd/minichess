/** State.scala
  * Cody Shepherd
  */

/** This class represents a board state or position at some time t.
  *
  * Note that its "value" field is the only one that is mutable - this is to facilitate
  * ttable functionality.
  * */
class State(val on_move: Player, var b_value: Double = 0, var w_value: Double = 0, val pieces: List[Piece]){
  //XXX: Potentially add other fields in support of heuristic evaluation

  override def toString: String = {
    var str = ""

    var tempstr = ""
    for (r <- List.range(0, Params.rows)){
      for (c <- List.range(0, Params.cols)) {
        val pc = pieces.find((x:Piece) => if (x.getLoc.x == r && x.getLoc.y == c) true else false)
        pc match {
          case Some(a) => a.toString    //TODO: Test this
          case _ => tempstr += "."
        }
      }
      str = tempstr + '\n' + str
      tempstr = ""
    }

    if(on_move == White())
      str = "W\n" + str
    else
      str = "B\n" + str

    str

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
          //System.err.println("States have different on_move")
          return false
        }
        if (this.b_value != s.b_value || this.w_value != s.w_value) {
          //System.err.println("States have different value")
          return false
        }

        if (this.pieces.length != s.pieces.length)
          return false

        for (tp <- this.pieces) {
          val j = s.pieces.find((x: Piece) => x == tp)
          j match {
            case Some(sp) => //System.err.println("matching piece found")
            case _ => return false
          }
        }
        true
      }
      case _ => {
        //System.err.println("Item not a State")
        false
      }
    }
  }
}

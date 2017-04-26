/** State.scala
  * Cody Shepherd
  */

/** This class represents a board state or position at some time t.
  *
  * Note that its "value" field is the only one that is mutable - this is to facilitate
  * ttable functionality.
  * */
class State(val on_move: Player, var value: Int = 0, val pieces: List[Piece]){

  override def toString: String = {
    //TODO: Change to pretty-print
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
        if (this.value != s.value) {
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

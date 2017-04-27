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

  def move(s: State): Move
}

case class AI(p: Player) extends Agent(p) {

  def move(s: State): Move = {
    new Noop()
  }

}

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
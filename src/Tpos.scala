/** Tpos.scala
  * minichess
  * Cody Shepherd
  */

/** This class represents an entry in the transposition table.
  * */
abstract class NodeType
case class Exact() extends NodeType
case class Upper() extends NodeType
case class Lower() extends NodeType

class Tpos(val score: Double, val depth: Int, val t: NodeType)


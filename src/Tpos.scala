/**
  * Created by codys on 5/12/2017.
  */
abstract class NodeType
case class Exact() extends NodeType
case class Upper() extends NodeType
case class Lower() extends NodeType

//class Tpos(val s: State, val score: Double, val depth: Int, val t: NodeType)
class Tpos(val score: Double, val depth: Int, val t: NodeType)


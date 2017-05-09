/**
  * Created by cody on 4/26/17.
  */

/** Kickstarter for beginning a game; allows configuration based on cmd line params
  *
  * telnet imcs.svcs.cs.pdx.edu 3589
  * */
class Terminal {

  // AI or human player
  // depth of search per AI player

  // name, pw
  // offer, accept

  // three-digit response codes from imcs server


  val cmds = Map(
    "remote" -> PartialFunction(remote)
  )

  def runtime(): Unit = {
    while(true){
      val lines = scala.io.StdIn.readLine().split(" ")
      if(checkCmd(lines.head))
        cmds(lines.head)(lines.tail)
    }
  }

  def checkCmd(s: String): Boolean = {
    cmds.keys.find((p: String) => p == s) match {
      case Some(a) => true
      case None => false
    }
  }

  def remote(args: Array[String]): Boolean = {
    val c = new Comms()

  }

}

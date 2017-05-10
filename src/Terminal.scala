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
    "remote" -> PartialFunction(remote),
    "exit" -> PartialFunction(leave)
  )

  def runtime(): Unit = {
    while(true) {
      val lines = scala.io.StdIn.readLine("> ").split(" ")

      if (checkCmd(lines.head))
        cmds(lines.head)(lines.tail)
      else
        System.out.println("Command not recognized.")
    }
  }

  def checkCmd(s: String): Boolean = {
    cmds.keys.find((p: String) => p == s) match {
      case Some(a) => true
      case None => false
    }
  }

  def leave(args: Array[String]): Unit = {
    System.exit(0)
  }

  def remote(args: Array[String]): Boolean = {
    val c = new Comms()

    if(args.length < 3){
      System.out.println("Usage: remote [uname] [passwd] [color]")
      return false
    }

    if(!c.connect(args(0), args(1))) {
      System.out.println("Failed to connect.")
      return false
    }

    System.out.println("Connection successful.")

    c.offer(args(2).toUpperCase())

    true
  }
}

object Term {
  def main(args: Array[String]): Unit = {
    val t = new Terminal
    t.runtime()
  }
}


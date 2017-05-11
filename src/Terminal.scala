/**
  * Created by cody on 4/26/17.
  */

/** Kickstarter for beginning a game; allows configuration based on cmd line params
  *
  * telnet imcs.svcs.cs.pdx.edu 3589
  * */
class Terminal {

  // AI or human player

  // three-digit response codes from imcs server

  var uname: String = "yorick"
  var pword: String = "infinitejest"

  val cmds = Map(
    "offer" -> PartialFunction(offerRemote),
    "accept" -> PartialFunction(acceptRemote),
    "depth" -> PartialFunction(depth),
    "creds" -> PartialFunction(creds),
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

  def creds(args: Array[String]): Unit = {
    if(args.length != 2){
      System.out.println("Usage: creds [uname] [pword] // Note: defaults are 'yorick' and 'infinitejest' //")
      return
    }
    uname = args(0)
    pword = args(1)
  }

  def depth(args: Array[String]): Unit = {
    if(args.length != 1) {
      System.out.println("Usage: depth [Int] // Note: default is 6 //")
      return
    }
    Params.plyDepth = args(0).toInt
  }

  def leave(args: Array[String]): Unit = {
    System.exit(0)
  }

  def offerRemote(args: Array[String]): Unit = {
    val c = new Comms()

    if(args.length != 1){
      System.out.println("Usage: offer [color]")
      return
    }

    if(!c.connect(uname, pword)) {
      System.out.println("Failed to connect.")
      return
    }

    System.out.println("Connection successful.")

    c.offer(args(0).toUpperCase())
  }

  def acceptRemote(args: Array[String]): Unit = {
    val c = new Comms()

    if(args.length != 1){
      System.out.println("Usage: accept [game id]")
      return
    }

    if(!c.connect(uname, pword)) {
      System.out.println("Failed to connect.")
      return
    }

    System.out.println("Connection successful.")

    c.accept(args(0).toUpperCase())
  }
}


object Term {
  def main(args: Array[String]): Unit = {
    val t = new Terminal
    t.runtime()
  }
}


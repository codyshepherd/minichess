import java.io.PrintStream
import java.net.{InetAddress, Socket}
import scala.io.BufferedSource

/**
  * Created by cody on 5/1/17.
  */

/** Socket Communications layer for talking to imcs server
  * */
class Comms {

  val server = "imcs.svcs.cs.pdx.edu"
  val port = 3589
  val protocol = "telnet"

  val sock  = new Socket(InetAddress.getByName(server), port)
  lazy val in = new BufferedSource(sock.getInputStream).getLines()
  val out = new PrintStream(sock.getOutputStream)

  var gamenum = ""

  def connect(u: String, p: String): Boolean = {

    in.next()         // dump initial connection string

    out.println("me " + u + " " + p)
    out.flush()

    val recvd = in.next()
    System.out.println(recvd)

    if(recvd.split(" ").head != "201") {
      return false
    }
    true
  }

  def offer(color: String): Boolean = {
    out.println("offer " + color)

    var recvd = in.next()
    System.out.println(recvd)

    if(recvd.split(" ").head != "103")
      return false

    recvd = in.next()
    System.out.println(recvd)
    while(recvd.split(" ").head != "105") {
      recvd = in.next()
      System.out.println(recvd)
    }

    System.out.println(in.next()) //dump the next line
    play()

    true
  }

  //offer, accept as white & black


  def play(): Unit = {
    val player = AI(White())

    var move = ""

    //System.out.println("dumping: " + in.next())   //dump newline
    var recvd = in.next()
    //System.err.println(recvd)
    var statestring = ""
    var state: Option[State] = None

    var acc = 0

    while(acc < 7){
      statestring = statestring + recvd + "\n"
      recvd = in.next()
      //System.err.println(recvd)
      acc += 1
    }
    //System.out.println(statestring)


    while(!List("230", "231", "232").contains(statestring.split(" ").head)){
      System.err.println("play loop, statestring: " + statestring)
      state = Some(Params.stringsToState(statestring.split("\n").toList))
      System.err.println("play loop, state: " + state.get.toString)
      move = player.move(state.get)
      System.out.println("My move: " + move)
      if(move == "x7-x7")
        return
      out.println(move)
      out.flush()

      System.err.println("dumping: " + in.next()) // timestamp
      System.err.println("dumping: " + in.next()) // opponent move
      System.err.println("dumping: " + in.next()) // empty space

      recvd = in.next()
      System.out.println(recvd)
      statestring = ""
      acc = 0
      while(acc < 7){
        statestring = statestring + recvd + "\n"
        recvd = in.next()
        System.out.println(recvd)
        acc += 1
      }
      //System.out.println(statestring)
    }


  }
  // uname: yorick ; pw: infinitejest

}

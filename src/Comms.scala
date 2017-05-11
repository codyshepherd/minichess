import java.io.PrintStream
import java.net.{InetAddress, Socket}
import java.util.NoSuchElementException

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

    try {
      in.next()         // dump initial connection string
      out.println("me " + u + " " + p)
      out.flush()

      val recvd = in.next()
      System.out.println(recvd)

      if(recvd.split(" ").head != "201")
        return false
    }
    catch {
      case e: NoSuchElementException => return false
    }

    true
  }

  def offer(color: String): Unit = {
    out.println("offer " + color)
    out.flush()

    var recvd = in.next()           // 103 [id] game waiting for offer acceptance
    System.out.println(recvd)

    if(recvd.split(" ").head != "103")
      return

    try {
      System.out.println(in.next() + "(dumped 0)")     // 105/106 W/B 5:00 game starts
      System.out.println(in.next() + "(dumped 1)")     // blank line
    }
    catch {
      case e: NoSuchElementException => return
    }

    if (color == "W"){

      play(White())
    }
    else if (color == "B"){
      try {
        System.out.println(in.next() + "(dumped 2)")     // opponent move
      }
      catch {
        case e: NoSuchElementException => return
      }

      play(Black())
    }
    else{
      System.out.println("Color not recognized.")
    }
  }

  def accept(id: String): Unit = {
    out.println("accept " + id)
    out.flush()

    var recvd = in.next()     // 105 / 106 W/B 5:00 5:00 game starts
    val tag = recvd.split(" ").head
    System.out.println(recvd)

    if(tag != "106" && tag != "105")
      return

    val p = if(tag == "106") Black() else White()
    if (p == White()){
      try {
        System.out.println(in.next() + "(dumped a0)")     // blank space
      }
      catch {
        case e: NoSuchElementException => return
      }
    }
    else{
      try {
        System.out.println(in.next() + "(dumped a1)")     // opponent move
        System.out.println(in.next() + "(dumped a2)")     // blank space
      }
      catch {
        case e: NoSuchElementException => return
      }
    }
    play(p)
  }

  def play(p: Player): Unit = {
    System.err.println("Player: " + p.toString)
    val player = AI(p)

    var move = ""
    var recvd = ""

    try {
      recvd = in.next()
    }
    catch {
      case e: NoSuchElementException => return
    }
    var statestring = ""
    var state: Option[State] = None

    var acc = 0

    while(acc < 7){
      statestring = statestring + recvd + "\n"
      try {
        recvd = in.next()
      }
      catch {
        case e: NoSuchElementException => return
      }
      System.out.println(recvd)
      acc += 1
    }
    //System.out.println(statestring)
    try {
      //System.out.println(in.next() + "(dumped 3)")   // blank space
      System.out.println(in.next() + "(dumped 4)")   // ? prompt
    }
    catch {
      case e: NoSuchElementException => return
    }

    while(!List("230", "231", "232").contains(statestring.split(" ").head)){
      System.err.println("My turn")
      //System.err.println("play loop, statestring: " + statestring)
      state = Some(Params.stringsToState(statestring.split("\n").toList))
      //System.err.println("play loop, state: " + state.get.toString)
      move = player.move(state.get)
      System.out.println("My move: " + move)
      if(move == "x7-x7")
        return
      out.println(move)
      out.flush()

      try {
        System.out.println(in.next() + "(dumped 5)")    // opponent move
        System.out.println(in.next() + "(dumped 6)")   //  blank space
        recvd = in.next()                               // first line of state
        System.out.println(recvd)
      }
      catch {
        case e: NoSuchElementException => return
      }

      statestring = ""
      acc = 0
      while(acc < 7){
        statestring = statestring + recvd + "\n"
        try {
          recvd = in.next()
        }
        catch {
          case e: NoSuchElementException => return
        }
        System.out.println(recvd)
        acc += 1
      }
      try {
        System.out.println(in.next() + "(dumped 7)")    // blank space
      }
      catch {
        case e: NoSuchElementException => return
      }
    }


  }
}

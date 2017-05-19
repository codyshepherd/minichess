/** Comms.scala
  * minichess
  * Cody Shepherd
  * */

import java.io.PrintStream
import java.net.{InetAddress, Socket}
import java.util.NoSuchElementException

import scala.io.BufferedSource

/** This class is the socket communications layer for talking to imcs server.
  * */
class Comms {

  val server = "imcs.svcs.cs.pdx.edu"
  val port = 3589
  val protocol = "telnet"

  val sock  = new Socket(InetAddress.getByName(server), port)
  lazy val in = new BufferedSource(sock.getInputStream).getLines()
  val out = new PrintStream(sock.getOutputStream)

  var gamenum = ""

  /** Returns whether or not a connection has been made to the imcs server using the
    * given username (u) and password (p)
    * */
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

  /** Offers a game of a specific color on the imcs server over a live connection.
    * */
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

  /** Accepts a game of a specific id number on the imcs server over a live connection.
    * */
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

  /** Creates two AI players, reading input from the imcs server and feeding it to
    * one or both players until the connection is closed.
    *
    * The second player is used if thinking on opponent's turn (Params.think) is enabled.
    * The second player has two fewer seconds to think than the first player and is
    * used primarily for populating the ttable.
    * */
  def play(p: Player): Unit = {
    System.err.println("Player: " + p.toString)
    val player = AI(p, Params.turnTime)
    val thinker = AI(p.opposite, Params.turnTime-2)

    var move = ""
    var throwaway = ""
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

      val newstate = Params.m.go(state.get)
      if(Params.think)
        try {
          throwaway = thinker.move(newstate)
        }
      catch {
        case e: Exception => throwaway = ""
      }

      try {
        System.out.println(in.next() + "(dumped 5)")    // opponent move
        System.out.println(in.next() + "(dumped 6)")   //  blank space
        recvd = in.next() // first line of state
        System.out.println(recvd)
      }
      catch {
        case e: Exception => return
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

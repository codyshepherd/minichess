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

  def connect(): Boolean = {
    val s = new Socket(InetAddress.getByName(server), port)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())

  }

  //offer, accept as white & black

  // uname: yorick ; pw: infinitejest

}

/**
  * Created by cody on 4/26/17.
  */

/** Performs (unit) testing on codebase.
  * */
class Tester {
  //read series of states and moves from files

  def testPawn(): Unit = {

  }

  def testQueen(): Unit = {

  }

  def testAll(): Unit = {
    testPawn()
  }

}

object Testing {
  def main(args: Array[String]): Unit = {
    val t = new Tester()
    print(t.testAll())
  }
}
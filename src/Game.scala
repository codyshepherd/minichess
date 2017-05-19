
/**
  * Created by cody on 4/26/17.
  */

/** This is the game Referee; i.e. it pits two Agents against one another.
  *
  * Game "owns" the board; it takes a move from each player on its turn and updates the
  * board accordingly.
  *
  * Keeps track of time
  *
  * Responsible for receiving moves and checking their legality before applying them to the
  * global game state
  * */
class Game(p1: Agent, p2: Agent) {

  //val path = "/Users/cody/IdeaProjects/minichess/src/teststates/"
  val path = "C:\\Users\\codys\\IdeaProjects\\minichess\\src\\teststates\\"
  //val path = "/u/cls9/IdeaProjects/minichess/src/teststates/"

  val s: State = Params.stateFromFile(path ++ "pawnfwd.txt")



  //print(s.toString)


}



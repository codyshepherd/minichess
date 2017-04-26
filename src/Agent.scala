/**
  * Created by cody on 4/26/17.
  */

/** This class represents the AI Player that makes moves and plays the game.
  * */
class Agent {
  //Keep track of board piece value as you go -- state.value?

  //heuristics for move ordering:
    // Material Value
    // Queen capture
    // Control the center
    // Pawn formation
    // Pawn advancement / close to promotion

  // dynamically increase search depth over time
    // use remembered value from current depth -1
    // use with ttable to amortize work done
    // "Iterative deepening"
}

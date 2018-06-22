# Minichess Player

## Organization

This program was written using an object-oriented approach, combined with functional-style list processing, probably
to the detriment of performance. 

The major components include: board representation, Agent, terminal, and communications.

The board is represented as a list of pieces along with some parameters. A board will generate all the moves available
to the side on-move; each piece in turn will generate its own move. Performance is lost during move generation,
specifically when pieces are searching their move footprint for legal moves. Each piece must search the entire list
of pieces for each square it wants to check. This could have been corrected by generating a 2-D array representation 
of the board during move generation, but I didn't have enough time to implement this.

The Agent uses iterative deepening to perform negamax search with alpha-beta pruning on a given state to find the
best move it can. It skips odd depths to save time. This would be more of a problem if the program were hitting 
depths greater than six or eight per turn (within 5 seconds constraint), but I never get deeper than that so 
I feel this is a fair trade.

The terminal allows the user to adjust play parameters between games. This includes changing the state evaluation 
heuristic, the time alloted per turn, toggling thinking-on-opponent's-time, toggling the transposition table, 
offering and accepting games (of either color), changing the program's login credentials for imcs, changin the 
maximum search depth, and evaluating a single state from a file without having to connect to imcs.

The communications layer connects with the imcs server, and instantiates an Agent to play a game. It manages
parsing of imcs data and responses.

## Features

- Negamax search with alpha-beta pruning and narrow-window search
- Iterative Deepening
- Transposition table with zobrist hashing
- Thinking on opponent's time
- Offering and accepting games of either color on imcs
- "One-shot" evaluation: reading a state from a file and searching it within given constraints for best move
- Material value or MV + mobility heuristic for state evaluation

### Experimental

Some of the features that I played around with included:

- turn-window "annealing." My early approaches had turn time increasing with the number of moves taken during a game. This
would in theory give the AI more time to think later on, when moves were more "important." I took this out because the
division required during this step degraded performance.

- Shuffling of moves before heuristic sort. The goal here was to add some nondeterminism to the tree search to prevent the
AI from getting stuck in a loop with another computer player. It was somewhat effective, but ultimately was removed in 
attempt to boost performance.

### Not Included due to time constraints

- Opening Book / Endgame Database
- A better state evaluation heuristic
- Probably other things I've forgotten.

## Performance

Given a five-second turn window, my program will be able to get depth 6 search out of its iterative deepening algorithm.
This is not great and primarily due to some asymptotic behavior in move generation. It could be fixed, but I don't have enough
time. See above discussion of state representation in the Organization section for more details.

The program otherwise plays correctly and doesn't have any real blind spots that I know of. It communicates with the imcs
server without problem, although it cannot serve as an intermediate "terminal" for imcs, i.e. it can only accept and offer
games, and cannot send any other imcs commands, nor interpret them.

## Bugs

The only real bug in the program (that I know of at this time) is that it can sometime get into a loop with another, 
similarly-programmed player. Two AI players might both simply move their King back and forth into a draw because neither 
finds a novel approach to their move evaluation. 

## Testing and Evaluation

I included unit tests. Coverage is not complete. Unit testing started off as systematic (i.e. my goal was to test everything
as I built it), but because of time constraints turned into "testing as-needed." Many of the tests in the testing class
were put in as part of investigations into bugs. A directory of board states is included to facilitate those tests.

Otherwise, testing was done by playing many games on imcs, both with other players and with the tackling dummy. I don't know who
owns Tormund, but I beat that bot one time! Take that!

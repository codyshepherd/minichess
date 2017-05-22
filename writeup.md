Cody Shepherd

CS 542

Homework 3 - Working Player

##Program Description

I took a mostly object-oriented approach to writing this program. The end result is slower than I would
like, so the remainder of this class will be about optimizing it to make it faster. The performance I am
seeing is likely the result of a combination of program design and language (Scala) application to this
domain.

As it is, I have implemented the following features in this player:

*   negamax search
*   alpha-beta pruning
*   transposition table
*   iterative deepening
*   thinking on opponent's time
*   multiple state evaluation heuristics (material value & mobility)

The user can toggle the use of the transposition table and thinking on opponents time, as well as set
the depth and time of search, between games at the terminal I wrote for the program.

I found that the most difficult thing about getting this player to play reasonably well was ensuring
that the move generation/execution and state evaluation were totally correct. The more "advanced" features
seemed much easier to implement. Though, on my desktop computer at home I am averaging 6 plies worth 
of depth per turn in 5 seconds. I don't know how bad this is, but it at least allows me to beat the
tackling dummy.

##Runs against Tackling Dummy

I have run many games against the tackling dummy, and won the requisite two with my current build. However,
that was last night during testing, and I failed to write down the game ID numbers. And it seems that
the tackling dummies are no longer being populated to the imcs server at this time (friday 3pm), so I 
will not be able to furnish any game numbers. 

I can furnish these at the soonest possible date that the tackling dummies are back up; I am also confident
that if this program is compiled and run on a tester's computer, it will beat the tackling dummy (though I
mae no claims against any human players -- for all the features built into this program it is not really
that good of a player [crying face]).
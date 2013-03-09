# Hagl: Haskell Game Language #

## Description ##

A DSEL for experimental game theory (also called evolutionary game theory and
behavioral economics).  Supports defining games and strategies, then executing
them repeatedly in order to collect and observe the results.

Hagl provides built-in support for standard game representations, such as
normal and extensive form, and constructs for defining games in terms of the
manipulation of a shared state (as in tic-tac-toe).  New game representations
can be easily added by instantiating a type class.

A monadic strategy DSL supports concise and vaguely English-like definitions of
strategies for playing these games, usually iteratively.

While Hagl provides some basic game analyses, its primary focus is simulation
and experimentation.  Games can be executed and strategies can be pitted
against each other for repeated play and in various kinds of tournaments.


## Examples ##

To play with the examples, load them into GHCi by running `ghci Hagl.Examples`
from this directory.

There are examples of Normal Form, Extensive Form, and State-Driven games.

For normal form games, you can view the payoff matrix by just evaluating the
game in GHCi.

For example:

    >> pd
        |  C  |  D 
      C | 2,2 | 0,3
      D | 3,0 | 1,1
    
You can view the game tree for any discrete game by using the `gameTree` function.

    >> gameTree pd
    Player 1
    +- C -> Player 2
    |  +- C -> [2,2]
    |  `- D -> [0,3]
    `- D -> Player 2
       +- C -> [3,0]
       `- D -> [1,1]

The function `execGame` is used to execute a game.  It takes a game, a set of
players to play the game, and a function to execute within the game execution
monad.

Execution functions include:

 * `step` - Process a single node in the game tree.
 * `once` - Run through the game a single time to completion.
 * `times n` - Run the full game `n` times.

There are also many printing functions available for inspecting the execution
state.  These can be found in `Hagl.Print.hs`.  Some examples
include:

 * `printMovesFromHere` - Print the available moves from this location in the game tree.
 * `printTranscripts` - Print the transcript of all completed games.
 * `printScore` - Print the current score.

The execution and printing functions can be executed sequentially via bind
operations.  The best way to illustrate how this all works together is a few
examples.

The following processes the first node in the game tree (Player 1's decision),
then prints the moves available to Player 2.
    
    >> execGame pd [tft, pavlov] (step >> printMovesFromHere)

The following runs the game four times, prints the transcript of all
iterations, and then prints the current (in this case, final) score.

    >> execGame pd [tft, suspicious] (times 4 >> printTranscripts >> printScore)

And finally, the following runs the game three times, prints the score, then
runs 100 more iterations and prints the final score.

    >> execGame pd [ccd, grim] (times 3 >> printScore >> times 100 >> printScore)

There is also a "tournament" facility that eases running many combinations of
players and comparing their final scores.  The tournament running functions
can be found in `Hagl.Tournament.hs`.

All tournament functions return their results as a list of player names paired
with scores.  This can be printed in a nice way by passing this result to the
`printResults` function, as shown below.

    >> roundRobin pd 2 [tft, titForTwoTats, grim, suspicious, pavlov] (times 100 >> printScore) >>= printResults

For each pair of players from the list, this will run the iterated prisoner's
dilemma 100 times, and print the score for those players.  At the end, the
total scores of all players involved will be printed in sorted order.

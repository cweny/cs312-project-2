# cs312-project-2
UBC CS312 Fall 2107 project 2

Authors: Carlos, Juliane, Nicholas

## What is the problem?
Implement an interactive, classic game of Othello in Haskell. We will be reimplementing our Prolog project.

The game is played on an 8 x 8 board, 64 discs and two players. The player must place a disc that outflanks the opposing player's discs, this flips the outflanked disc(s) to the player's color. The winner of the game is the player with the majority of discs of their color on the board.

The program will automatically enforce Othello's rule-set, which can be found here, http://www.hannu.se/games/othello/rules.htm

## What is the something extra?
In Project 1, we had a problem with our AI using too much memory and crashing at higher strengths. Therefore, we will attempt to fix this problem in Project 2 using Haskell.

Our game will allow the player to choose between three different playing modes, player vs player, player vs AI, and AI vs. AI game. Our computer AI will decide a move based on the number of discs they can flip of their own color. This decision will be made according to the Minimax rule.The goal of Minimax is to minimize the maximum loss by calculating and comparing the "values" of possible moves. In our program, "values" refers to the number of each player's discs on the board after a move is made. The AI will then choose the move that maximizes player's values and minimizes it's own values. This page was used as reference, https://en.wikipedia.org/wiki/Minimax.

In the AI vs. AI mode the "player" can spectate a between two AI. The player can choose the strength for each AI, and how frequently they want to print out the moves. This was mostly implemented for debugging purposes, as manually inputting 60 moves to reach game over can be very tedious.

## What did we learn from doing this?
Functional programming is suitable for grid-based games like Othello. In our Prolog version from Project 1, we had troubles with regards to run time and stack overflow because our AI used breadth-first search.

This time we used depth-first search, and Haskell's lazy evaluation made this a natural fit. Haskell also has several programming features which allowed us to code more concisely, ending at about half the number of code lines. Finally, we improved the AI's heuristics to use move mobility instead of raw point advantage, which makes it stronger.

Debugging was again an issue, but Haskell was much better at printing large objects to the screen, so that was an improvement.

The final result is that our AI no longer has any practical memory limitations, and can completely games at search depth ~5 in a reasonable time. Overall, Haskell seems much better suited to programming a simplistic AI than Prolog.
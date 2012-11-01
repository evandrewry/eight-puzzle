#COMS W4701 - Artificial Intelligence
#Programming Assignment #2 - Puzzle
#Evan Drewry - ewd2106

#Assignment
===========
Using the general search and graph search code as presented in class,
implement an appropriate enqueuing function for the A\* algorithm. 
Make use of and implement any enhancements you may need for the node
structures.

Implement a problem representation (including state representation,
successor function, same state test, and goal test) for the 8-puzzle.

Implement both heuristic functions shown in class (i.e. misplaced
tiles and Manhattan distance).

Instrument the A\* code to accumulate into a global variable
\*nodes-expanded\* the total number of nodes expanded.

Implement code to generate random solvable initial states.

Run the code to solve 5 random intial states using both heuristics
and compare the results.

Extra Credit (3 points): Develop another consistent heuristic
function that dominates Manhattan distance; run it on the same
random states and compare the results.

#Implementation


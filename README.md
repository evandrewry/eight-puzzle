Programming Assignment #2 - Puzzle
======================================================================
Due 7 Nov 2012

COMS W4701 - Artificial Intelligence

Evan Drewry - ewd2106

NOTE: I finished the assignment before the email about standardizing
the format, but have implemented wrapper functions that allow my
implementation to conform to the given spec, resulting in a few
repetitive/weird functions.


Assignment
======================================================================
Using the general search and graph search code as presented in class,
implement an appropriate enqueuing function for the A\* algorithm. 
Make use of and implement any enhancements you may need for the node
structures.

Implement a problem representation (including state representation,
successor function, same state test, and goal test) for the 8-puzzle.

Implement both heuristic functions shown in class (i.e. misplaced
tiles and Manhattan distance).

Instrument the A\* code to accumulate into a global variable
`*nodes-expanded*` the total number of nodes expanded.

Implement code to generate random solvable initial states.

Run the code to solve 5 random intial states using both heuristics
and compare the results.

Extra Credit (3 points): Develop another consistent heuristic
function that dominates Manhattan distance; run it on the same
random states and compare the results.



Implementation
======================================================================
My implementation of A\* search is an extension of the general search
code covered in class. The `(a-star-search initial-state goalp samep
heuristic)` function calls general search with a priority queue for
the fringe with key
```lisp
(lambda (node)                                                
  (+ (funcall heuristic node)                                 
     (node-path-cost node)))
```
which is the `f(n) = g(n) + h(n)` function associated with A\* search.

To see a trial run of the A\* search (using both heuristics, with five
trials each) load the eight-puzzle.lisp file and enter
`(run-tests)`
into the REPL. When each trial runs, it will print the resulting value
of \*nodes-expanded\* along with some informative text about which
trial and heuristic was just run.

The goal state is defined in `*goal*` as a simple lisp list of the
digits 0-9, where 0 represents the blank square. By default, this is
defined to be `'(0 1 2 3 4 5 6 7 8 9)` but can be redifined as you
please. The `goalp` function simply calls the `samep` function on the
input state and the `*goal*` variable.

The actual puzzle search functions are implemented as 
`(manhattan-distance-search initial-state)` and
`(misplaced-tiles-search initial-state)` and an initial state can be
generated by calling `(generate-puzzle)` which starts with the goal
and then works backwards randomly to create a scrambled puzzle. These
two functions return a list of actions (MOVE UP, MOVE DOWN, etc.)
that comprise the path found by the A\* search. The directions in
these actions should be interpreted not as which direction to slide
the tile in, but rather which direction the blank spot should move
towards. For example, MOVE DOWN should be interpreted as "slide tile
up into the blank spot, so that the blank spot is now one spot below
where it was before".

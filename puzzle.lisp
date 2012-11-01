(provide "puzzle")
(require "search" "search.lisp")

(defvar *goal* '(0 1 2 3 4 5 6 7 8))

(defun eight-puzzle-samep (a b)
  (equal a b))

(defun eight-puzzle-goalp (puzzle)
  (eight-puzzle-samep puzzle *goal*))

(defun swap-tiles (puzzle i j)
  "Swaps the tiles at i and j."
  (let ((puz (copy-list puzzle)))
    (rotatef (nth i puz) (nth j puz))
    puz))

(defun zero-index (puzzle)
  "Returns the position of the zero in the puzzle"
  (position 0 puzzle))

(defun on-top-edge-p (puzzle)
  (let ((pos (zero-index puzzle)))  
    (or (= 0 pos)
        (= 1 pos)
        (= 2 pos))))

(defun move-up-state (puzzle)
  "Gives the state (or nil if there is none) after moving our
  blank space 1 spot up"
  (unless (on-top-edge-p puzzle)
    (let ((pos (zero-index puzzle)))
      (swap-tiles puzzle pos (- pos 3)))))

(defun move-up-triple (puzzle)
  "Gives the action-state-cost triple (or nil if there is none)
  for moving our blank space 1 spot up"
  (let ((state (move-up-state puzzle)))
    (unless (null state)
      (list (list 'move 'up) state 1))))

(defun on-right-edge-p (puzzle)
  (let ((pos (zero-index puzzle)))  
    (or (= 2 pos)
        (= 5 pos)
        (= 8 pos))))

(defun move-right-state (puzzle)
  "Gives the state (or nil if there is none) after moving our
  blank space 1 spot to the right"
  (unless (on-right-edge-p puzzle)
    (let ((pos (zero-index puzzle)))
      (swap-tiles puzzle pos (1+ pos)))))

(defun move-right-triple (puzzle)
  "Gives the action-state-cost triple (or nil if there is none)
  for moving our blank space 1 spot to the right"
  (let ((state (move-right-state puzzle)))
    (unless (null state)
      (list (list 'move 'right) state 1))))

(defun on-left-edge-p (puzzle)
  (let ((pos (zero-index puzzle)))  
    (or (= 0 pos)
        (= 3 pos)
        (= 6 pos))))

(defun move-left-state (puzzle)
  "Gives the state (or nil if there is none) after moving our
  blank space 1 spot to the left"
  (unless (on-left-edge-p puzzle)
    (let ((pos (zero-index puzzle)))
      (swap-tiles puzzle pos (1- pos)))))

(defun move-left-triple (puzzle)
  "Gives the action-state-cost triple (or nil if there is none)
  for moving our blank space 1 spot to the left"
  (let ((state (move-left-state puzzle)))
    (unless (null state)
      (list (list 'move 'left) state 1))))

(defun on-bottom-edge-p (puzzle)
  (let ((pos (zero-index puzzle)))  
    (or (= 6 pos)
        (= 7 pos)
        (= 8 pos))))

(defun move-down-state (puzzle)
  "Gives the state (or nil if there is none) after moving our
  blank space 1 spot down"
  (unless (on-bottom-edge-p puzzle)
    (let ((pos (zero-index puzzle)))
      (swap-tiles puzzle pos (+ pos 3)))))

(defun move-down-triple (puzzle)
  "Gives the action-state-cost triple (or nil if there is none)
  for moving our blank space 1 spot down"
  (let ((state (move-down-state puzzle)))
    (unless (null state)
      (list (list 'move 'down) state 1))))

(defun eight-puzzle-successor (puzzle)
  "Returns action-state-cost triples for eight-puzzle"
  (remove nil (list (move-up-triple puzzle)
                    (move-right-triple puzzle)
                    (move-left-triple puzzle)
                    (move-down-triple puzzle))))

(defun random-element (list)
  (nth (random (length list)) list))

(defun generate-puzzle-inner (puzzle max-depth)
  (if (= max-depth 0)
    puzzle
    (generate-puzzle-inner
      (random-element (remove nil (list (move-up-state puzzle)
                                        (move-right-state puzzle)
                                        (move-left-state puzzle)
                                        (move-down-state puzzle))))
      (1- max-depth))))

(defun generate-puzzle ()
  (generate-puzzle-inner *goal* 100))

(defun puzzle-search (puzzle heuristic)
  (a-star-search puzzle
                  #'eight-puzzle-successor
                  #'eight-puzzle-goalp
                  #'eight-puzzle-samep
                  heuristic))

(defun misplaced-tiles-heuristic (node)
  (let ((state (node-state node)))
    (loop for i from 0 to 8
          count (funcall (complement #'=) (elt state i) (elt *goal* i)))))

(defun misplaced-tiles-search (puzzle)
  (puzzle-search puzzle #'misplaced-tiles-heuristic))

(defun xcoord (index)
  (mod index 3))

(defun ycoord (index)
  (/ index 3))

(defun manhattan-distance (a b)
  (+ (abs (- (xcoord a) (xcoord b)))
     (abs (- (ycoord a) (ycoord b)))))

(defun manhattan-distance-heuristic (node)
  (let ((state (node-state node)))
    (loop for i from 0 to 8
          sum (manhattan-distance (elt state i) (elt *goal* i)))))

(defun manhattan-distance-search (puzzle)
  (puzzle-search puzzle #'manhattan-distance-heuristic))


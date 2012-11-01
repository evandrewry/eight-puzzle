;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMS W4701 Assignment #2: EIGHT-PUZZLE
;;; Evan Drewry - ewd2106 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HEAP CODE FOR PRIORITY QUEUE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i]
   may be larger than its children. If it is, moves heap[i]
   down where it belongs."
  (unless (heap-leafp heap i)
    (let ((left-index (heap-left i))
          (right-index (heap-right i)))
      (let ((smaller-index
              (if (and (< right-index (length heap))
                       (< (heap-val heap right-index key)
                          (heap-val heap left-index key)))
                right-index
                left-index)))
        (when (> (heap-val heap i key)
                 (heap-val heap smaller-index key))
          (rotatef (elt heap i)
                   (elt heap smaller-index))
          (heapify heap smaller-index key))))
    ))

(defun heap-pop (heap key)
  "Pops the best (lowest valued) item off the heap."
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-find-pos (heap i val key)
  "Bubbles up from i to find position for val, moving items
   down in the process"
  (cond ((or (zerop i)
             (< (heap-val heap (heap-parent i) key) val))
         i)
        (t (setf (elt heap i) (elt heap (heap-parent i)))
           (heap-find-pos heap (heap-parent i) val key))
        ))

(defun heap-insert (heap item key)
  "Puts an item into a heap"
  (vector-push-extend nil heap)
  (setf (elt heap (heap-find-pos heap (1- (length heap))
                                 (funcall key item) key))
        item)
  )

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; QUEUE CODE FOR SEARCH ALGORITHMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct q
  (enqueue #'enqueue-FIFO)
  (key #'identity)
  (last nil)
  (elements nil))

(defun q-emptyp (q)
  "Returns T if queue is empty."
  (= (length (q-elements q)) 0))

(defun q-front (q)
  "Returns the element at the front of the queue."
  (elt (q-elements q) 0))

(defun q-remove (q)
  "Removes and returns the element at the front of the queue."
  (if (listp (q-elements q))
    (pop (q-elements q))
    (heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
  "Inserts the items into the queue, according to the
   queue's enqueuing function. Returns the altered queue."
  (funcall (q-enqueue q) q items)
  q)

(defun enqueue-LIFO (q items)
  "Adds a list of items to the front fo the queue."
  (setf (q-elements q) (nconc items (q-elements q)))
  items)

(defun enqueue-FIFO (q items)
  "Adds a list of items to the end of the queue."
  (if  (q-emptyp q)
    (setf (q-elements q) items)
    (setf (cdr (q-last q)) items))
  (setf (q-last q) (last items))
  items)

(defun enqueue-priority (q items)
  "Inserts the items by priority of key values."
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  (mapc (lambda (item)
          (heap-insert (q-elements q) item (q-key q)))
        items)
  items)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENERAL SEARCH CODE & ALGORITHMS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *nodes-expanded* 0)

(defstruct node 
  (state nil)
  (parent nil)
  (action nil)
  (path-cost 0)
  (depth 0))

(defun action-sequence (node &optional (actions nil))
  (if (node-parent node)
    (action-sequence (node-parent node)
                     (cons (node-action node) actions))
    actions))

(defun expand (successor node)
  "Increments *nodes-expanded* and expands the node using the
   successor function."
  (incf *nodes-expanded*)
  (let ((triples (funcall successor (node-state node))))
    (mapcar (lambda (action-state-cost)
              (let ((action (car action-state-cost))
                    (state (cadr action-state-cost))
                    (cost (caddr action-state-cost)))
                (make-node :state state
                           :parent node
                           :action action
                           :path-cost (+ (node-path-cost node)
                                         cost)
                           :depth (1+ (node-depth node)))
                ))
            triples)
    ))

(defun tree-search (fringe successor goalp)
  "General tree search."
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (if (funcall goalp (node-state node))
        (action-sequence node)
        (tree-search (q-insert fringe (expand successor node))
                     successor goalp))
      )))

(defun graph-search (fringe closed successor goalp samep)
  "General tree search."
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (cond ((funcall goalp (node-state node))
             (action-sequence node))
            ((member (node-state node) closed
                     :test samep :key #'node-state)
             (graph-search fringe closed
                           successor goalp samep))
            (t (let ((successors (expand successor node)))
                 (graph-search (q-insert fringe successors)
                               (cons node closed)
                               successor goalp samep)))
            ))
    ))

(defun general-search (initial-state successor goalp
                       &key (samep #'eql)
                            (enqueue #'enqueue-LIFO)
                            (key #'identity))
  "General search."
  (setf *nodes-expanded* 0)    
  (let ((fringe (make-q :enqueue enqueue :key key)))
    (q-insert fringe (list (make-node :state initial-state)))
    (graph-search fringe nil successor goalp samep)))

(defun a-star-search (initial-state successor goalp samep heuristic)
  "General A* search."
  (general-search initial-state successor goalp
                  :samep samep
                  :enqueue #'enqueue-priority
                  :key (lambda (node)
                         (+ (funcall heuristic node)
                            (node-path-cost node)))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EIGHT-PUZZLE CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *goal* '(0 1 2 3 4 5 6 7 8))

(defun eight-puzzle-samep (a b)
  "samep function for puzzle state"
  (equal a b))

(defun eight-puzzle-goalp (puzzle)
  "goalp function for puzzle state"
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
  "Checks if blank spot is on the top edge"
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
  "Checks if blank spot is on the right edge"
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
  "Checks if blank spot is on the left edge"
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
  "Checks if blank spot is on the bottom edge"
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
  "Picks a random element from the input list."
  (nth (random (length list)) list))

(defun generate-puzzle-inner (puzzle max-depth)
  "Generates a random initial state by starting at the goal and
   working backwards"
  (if (= max-depth 0)
    puzzle
    (generate-puzzle-inner
      (random-element (remove nil (list (move-up-state puzzle)
                                        (move-right-state puzzle)
                                        (move-left-state puzzle)
                                        (move-down-state puzzle))))
      (1- max-depth))))

(defun generate-puzzle ()
  "Generates a random initial state by starting at the goal and
   working backwards"
  (generate-puzzle-inner *goal* 100))

(defun puzzle-search (puzzle heuristic)
  "General A* puzzle solver; requires heuristic function to be passed in."
  (a-star-search puzzle
                  #'eight-puzzle-successor
                  #'eight-puzzle-goalp
                  #'eight-puzzle-samep
                  heuristic))

(defun misplaced-tiles-heuristic (node)
  "Implentation of misplaced tiles heuristic."
  (let ((state (node-state node)))
    (loop for i from 0 to 8
          count (funcall (complement #'=) (elt state i) (elt *goal* i)))))

(defun misplaced-tiles-search (puzzle)
  "A* search for puzzle goal from input puzzle using the
   misplaced tiles heuristic."
  (puzzle-search puzzle #'misplaced-tiles-heuristic))

(defun xcoord (index)
  "x-coordinate of the index on the 3x3 board."
  (mod index 3))

(defun ycoord (index)
  "y-coordinate of the index on the 3x3 board."
  (/ index 3))

(defun manhattan-distance (a b)
  "Computes manhattan distance between index a and index b in the 
   list used for the state representation."
  (+ (abs (- (xcoord a) (xcoord b)))
     (abs (- (ycoord a) (ycoord b)))))

(defun manhattan-distance-heuristic (node)
  "Implentation of manhattan distance heuristic."
  (let ((state (node-state node)))
    (loop for i from 0 to 8
          sum (manhattan-distance (elt state i) (elt *goal* i)))))

(defun manhattan-distance-search (puzzle)
  "A* search for puzzle goal from input puzzle using the
   manhattan distance heuristic."
  (puzzle-search puzzle #'manhattan-distance-heuristic))

(defun run-tests ()
  "Runs 5 trials of each search and prints out results"
  (loop for i from 1 to 5
        do (let ((p (generate-puzzle)))
             (format t "Trial #~D~%" i)
             (misplaced-tiles-search p)
             (format t "  Misplaced tiles: ~D~%" *nodes-expanded*)
             (manhattan-distance-search p)
             (format t "  Manhattan distance: ~D~%" *nodes-expanded*))))

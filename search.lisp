(provide "search")
(require "queue" "queue.lisp")

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
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (if (funcall goalp (node-state node))
        (action-sequence node)
        (tree-search (q-insert fringe (expand successor node))
                     successor goalp))
      )))

(defun graph-search (fringe closed successor goalp samep)
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
  (setf *nodes-expanded* 0)    
  (let ((fringe (make-q :enqueue enqueue :key key)))
    (q-insert fringe (list (make-node :state initial-state)))
    (graph-search fringe nil successor goalp samep)))

(defun a-star-search (initial-state successor goalp samep heuristic)
  (general-search initial-state successor goalp
                  :samep samep
                  :enqueue #'enqueue-priority
                  :key (lambda (node)
                         (+ (funcall heuristic node)
                            (node-path-cost node)))))


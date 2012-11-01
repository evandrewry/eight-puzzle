(provide "queue")
(require "heap" "heap.lisp")

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




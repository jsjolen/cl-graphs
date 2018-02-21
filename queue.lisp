(in-package :cl-graphs)

(defstruct (queue (:constructor %make-queue))
  backing-array
  deqp enqp
  )
;; Circular queue
(defun make-queue (&optional (initial-size 25))
  (%make-queue
   :backing-array
   (make-array initial-size :adjustable t)
   :enqp 0 :deqp 0))
(defun enqueue (v q))
(defun dequeue (v q))

(in-package :cl-graphs)
(annot:enable-annot-syntax)
(defstruct (graph
	     (:constructor %make-graph))
  vertex-table test)

@export
(defun make-graph (&optional (test 'eql))
  (%make-graph
   :vertex-table
   (make-hash-table :test test)
   :test test))

(defun gelt (id g)
  (multiple-value-bind (v e) (gethash id (graph-vertex-table g))
    (values v e)))
(defun gelt-set (id g val)
  (setf (gethash id (graph-vertex-table g))
	val))
(defsetf gelt gelt-set)

@export
(defun add-vertex (id graph)
  (multiple-value-bind (_ ex?) (gethash id (graph-vertex-table graph))
    (declare (ignore _))
    (unless ex?
      (setf (gethash id (graph-vertex-table graph)) nil))))
(deftype direction ()
  '(:or :ltr :rtl :none))
(defstruct (edge
	     (:constructor %make-edge))
  weight direction friend)
(defun edge= (e1 e2)
  (equal e1 e2))

(defun ensure-unique-edge (e v g)
  (setf (gelt v g) (delete e (gelt v g) :test #'edge=)))

(declaim (inline extend))
(defun extend (v thing)
  (cons v thing))

@export
(defun add-edge (left right graph &optional (weight 0) (direction :ltr))
  (let ((e1 (%make-edge :weight weight :direction direction :friend right))
	(e2 (%make-edge :weight weight :direction direction :friend left)))
    (flet ((add-e1 ()
	     (ensure-unique-edge e1 left graph)
	     (setf (gelt left graph)
		   (extend e1 (gelt left graph))))
	   (add-e2 ()
	     (ensure-unique-edge e2 right graph)
	     (setf (gelt right graph)
		   (extend e2 (gelt right graph)))))
      (match (intern (symbol-name direction) :keyword)
	(:none
	 (add-e1)
	 (add-e2))
	(:ltr
	 (add-e1))
	(:rtl
	 (add-e2))))))

@export
(defun remove-edge (left right graph &optional (direction :any))
  (let ((les (gelt left graph))
	(res (gelt right graph)))
    (macrolet ((rem-form (from dir edge-name)
		 `(remove ,edge-name ,from
			   :test (lambda (e1 e2)
				   (etypecase e1
				     (edge
				      (and (eq e2 (edge-friend e1))
					   (eq ,dir (edge-direction e1))))
				     (symbol
				      (and (eq e1 (edge-friend e2))
					   (eq ,dir (edge-direction e2)))))))))
      (flet ((remove-ltr ()
	       (setf (gelt left graph)
		     (rem-form les :ltr right)))
	     (remove-rtl ()
	       (setf (gelt right graph)
		     (rem-form res :rtl left))))
	(match direction
	  (:any
	   (remove-ltr)
	   (remove-rtl))
	  (:ltr
	   (remove-ltr))
	  (:rtl
	   (remove-rtl))
	  (:none
	   (remove-ltr) (remove-rtl)))))))
@export
(defun -> (from to graph &optional (weight 0))
  (add-edge from to graph weight 'ltr))
@export
(defun <- (to from graph &optional (weight 0))
  (add-edge to from graph weight 'rtl ))
@export
(defun <-> (left right graph &optional (weight 0))
  (add-edge left right graph weight 'none))
@export
(defun edges (id g)
  (gelt id g))

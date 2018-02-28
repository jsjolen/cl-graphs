(in-package :cl-graphs)
(annot:enable-annot-syntax)
#|
https://github.com/yourbasic/graph
breadth-first DONE
depth-first search, DONE
topological ordering,
strongly and weakly connected components,
bipartion,
shortest paths,
maximum flow,
Euler walks,
minimum spanning trees and
Dijkstra's
|#
;; Would be nice if you can:
					; 1. trace paths (return path found)
					; 2. map paths (funcall every vertex visited)
					; 3. search for element

(defmacro defsearch (name constructor taker adder empty?)
  `(defun ,name (looking-for graph start)
     (let ((container (,constructor start))
	   (vt (make-hash-table :test (graph-test graph))))
       (flet ((seen? (v)
		(gethash v vt))
	      (see (v lv)
		(setf (gethash v vt) lv)))
	 (let (last-vert vert)
	   (loop while (not (,empty? container)) do
		(setf last-vert vert
		      vert (,taker container))
		(cond
		  ((funcall (graph-test graph) vert looking-for)
		   (see vert last-vert)
		   (return-from ,name (values t vt)))
		  ((not (seen? vert))
		   (see vert last-vert)
		   (map nil
			#'(lambda (e)
			    (,adder (edge-friend e) container))
			(edges vert graph))))))
	 (values nil vt)))))

@export
(defsearch depth-first-search list pop push null)
@export
(defsearch breadth-first-search
    (lambda (e)
      (let ((q (make-queue :simple-queue :minimum-size 10)))
	(qpush q e)))
  qpop
  (lambda (y x)
    (funcall #'qpush x y))
  (lambda (q) (= (qsize q) 0)))

(defun path (from to table test)
  (match (list from to table)
    ((list nil nil nil)
     nil)
    ((list from to table)
     (labels
	 ((rec (v acc)
	    (if (funcall test v from)
		(nreverse (cons v acc))
		(rec (gethash v table) (cons v acc)))))
       (rec to '())))))

(macrolet ((defpath (name fn)
	     `(defun ,name (looking-for graph start)
		(multiple-value-bind (found? table)
		    (,fn looking-for graph start)
		  (if found?
		      (path start looking-for table (graph-test graph))
		      (path nil nil nil (graph-test graph)))))))
  @export
  (defpath depth-first-path depth-first-search)
  @export
  (defpath breadth-first-path breadth-first-search))

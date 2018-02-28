(defpackage :cl-graphs-tests
  (:use :cl-graphs :cl :prove))

(in-package :cl-graphs-tests)

(subtest "[MUT] Graph construction and introspection"
  (let ((g (make-graph)))
    (add-vertex 'hi g)
    (add-vertex 'hello g)
    (add-edge 'hi 'hello g :direction :none)
    (is (length (edges 'hi g)) 1)
    (is (length (edges 'hello g)) 1)
    (add-vertex 'hi2 g)
    (add-vertex 'hello2 g)
    (add-edge 'hi2 'hello2 g :direction :ltr)
    (is (length (edges 'hi2 g)) 1)
    (is (length (edges 'hello2 g)) 0)))
(subtest "[MUT] Edge removal"
  (let ((g (make-graph)))
    (add-vertex 'hi g)
    (add-vertex 'hello g)
    (add-edge 'hi 'hello g :direction :none)
    (remove-edge 'hi 'hello g :any)
    (is (length (edges 'hi g)) 0)))
(subtest "[MUT] Depth-first search"
  (let ((g (make-graph)))
    (add-vertex 'hi g)
    (add-vertex 'hello g)
    (<-> 'hi 'hello g)
    (is (depth-first-search 'hello g 'hi) t)
    (is (depth-first-search 'hi g 'hello) t)))
(subtest "[MUT] Breadth-first search"
  (let ((g (make-graph)))
    (add-vertex 'hi g)
    (add-vertex 'hello g)
    (<-> 'hi 'hello g)
    (is (depth-first-search 'hello g 'hi) t)
    (is (depth-first-search 'hi g 'hello) t)))

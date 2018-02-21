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

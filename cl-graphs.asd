;;;; cl-graphs.asd

(asdf:defsystem #:cl-graphs
  :description "Describe cl-graphs here"
  :author "Johan Sjölén"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "mutable")
	       (:file "algos")
               (:file "cl-graphs"))
  :depends-on (:trivia
	       :alexandria
	       :queues
	       :cl-annot
	       :prove)
   :in-order-to ((test-op (test-op cl-graphs-test))))

(asdf:defsystem #:cl-graphs-tests
    :depends-on (:cl-graphs :prove)
    :defsystem-depends-on (:prove-asdf)
    :components
    ((:test-file "tests"))
    :perform (test-op :after (op c)
		      (funcall (intern #.(string :run) :prove))))

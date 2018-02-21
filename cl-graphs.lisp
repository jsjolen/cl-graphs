;;;; cl-graphs.lisp

(in-package #:cl-graphs)

(defmacro hole (v &optional (error? nil))
  `(progn (if ,error?
	     (error "Hole here")
	     (progn
	       #+sbcl
	       (warn "Hole in: ~A" (nth 0 (sb-debug:list-backtrace)))
	       #-sbcl
	       (warn "Hole somewhere")))
	  ,v)
  (warn "Hole compiled"))

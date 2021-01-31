(asdf:defsystem #:lledger
  :serial t
  :depends-on (#:cl-ppcre
	       #:cl-containers)
  :components ((:file "utilities.lisp")
	       (:file "package.lisp")
	       (:file "classes.lisp")))

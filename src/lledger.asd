(asdf:defsystem "lledger"
  :serial t
  :depends-on ("cl-ppcre"
	       "cl-containers")
  :components ((:file "utilities")
	       (:file "package")
	       (:file "classes")))

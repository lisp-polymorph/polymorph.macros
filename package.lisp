;;;; package.lisp

(defpackage #:polymorph.macros
  (:use #:cl
	      #:adhoc-polymorphic-functions
	      #:polymorph.utility
	      #:alexandria)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))

  (:export #:zapf
	         #:bind*
	         #:define-struct))

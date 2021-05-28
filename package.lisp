;;;; package.lisp

(defpackage #:polymorph.macros
  (:use #:cl
        #:polymorphic-functions
        #:alexandria)
  (:local-nicknames (:cm :sandalphon.compiler-macro)
                    (:mop :closer-mop))

  (:export #:zapf
           #:setf*
           #:bind*
           #:define-struct))

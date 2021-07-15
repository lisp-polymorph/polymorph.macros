;;;; package.lisp

(defpackage #:polymorph.macros
  (:use #:cl
        #:polymorphic-functions
        #:introspect-ctype
        #:alexandria)
  (:local-nicknames (:mop :closer-mop))

  (:export #:zapf
           #:setf*
           #:bind*
           #:define-struct))

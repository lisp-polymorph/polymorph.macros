;;;; package.lisp

(defpackage #:polymorph.macros
  (:use #:cl
        #:polymorphic-functions
        #:introspect-ctype
        #:alexandria)
        ;#:polymorph.copy-cast)
  (:local-nicknames (:mop :closer-mop))
  (:shadowing-import-from #:polymorph.copy-cast #:deep-copy)
  (:shadowing-import-from #:polymorph.maths #:=)
  (:export #:zapf
           #:setf*
           #:bind*
           #:bind
           #:def
           #:lambda*
           #:case=))

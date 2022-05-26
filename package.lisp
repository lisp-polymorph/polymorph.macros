;;;; package.lisp

(defpackage #:polymorph.macros
  (:use #:cl
        #:polymorphic-functions
        #:introspect-ctype
        #:alexandria)
        ;#:polymorph.maths)
        ;#:polymorph.copy-cast)
  (:local-nicknames (:mop :closer-mop))
  (:shadowing-import-from #:polymorph.maths #:=)
  (:shadowing-import-from #:polymorph.copy-cast #:deep-copy)
  (:export #:zapf
           #:setf*
           #:bind*
           #:bind
           #:def
           #:lambda*
           #:case=))

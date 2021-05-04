;;;; polymorph.utility.asd

(asdf:defsystem #:polymorph.utility
    :description "Macros and utility for polymorph.stl"
    :author "Commander Thrashdin"
    :license  "CCA"
    :version "0.5"
    :serial t
    :depends-on (#:adhoc-polymorphic-functions #:compiler-macro)
    :components ((:file "package")
                 (:file "polymorph.utility")))

;;;; polymorph.macros.asd

(asdf:defsystem #:polymorph.macros
    :description "Macros and utility for polymorph.stl"
    :author "Commander Thrashdin"
    :license  "CCA"
    :version "0.5"
    :serial t
    :depends-on (#:adhoc-polymorphic-functions #:polymorph.utility #:compiler-macro)
    :components ((:file "package")
                 (:file "polymorph.macros")))

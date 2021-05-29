;;;; polymorph.macros.lisp

(in-package #:polymorph.macros)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro zapf (place name expr &environment env)
    "Usage: setting element of a container in an efficient way.
  Name argument refers to the name that can be used in an expr instead of a palce value.
  Example: (zapf (gethash ht key) v (+ v (expt v v)))"
    (multiple-value-bind
          (temps exprs stores store-expr access-expr)
        (get-setf-expansion place)
      `(let* (,@(mapcar #'list temps exprs)
              (,(car stores)
                (let ((,name ,access-expr))
                  (declare (type ,(%form-type place env) ,name))
                  ,expr)))
         ,store-expr))))

(defmacro setf* (place val &environment env)
  "Usage: setting a place similar to setf, but in a type-safe way."

  (if (symbolp place)
      (if (subtypep (%form-type val env) (%form-type place env) env)
          `(setq ,place ,val)
          (error "Changing type of the variables is prohibited"))
      (multiple-value-bind (temps exprs stores store-expr access-expr)
          (get-setf-expansion place env)
        (declare (ignorable access-expr))
        (if temps
            `(let* ((,@temps ,@exprs)
                    (,@stores ,val))
               (declare (type ,(%form-type (car exprs) env) ,@temps)
                        (type ,(%form-type val env) ,@stores))
               ,store-expr)
            `(let* ((,@stores ,val))
               (declare (type ,(%form-type val env) ,@stores))
               ,store-expr)))))





(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro bind* (bindings &body body)
    "Bind unites 3 things: let*, multiple-value-bind and builtin type declarations.
   Uses default for filling out the values if types was provided, otherwise defaults to nil.
Examples of usage:

(bind* ((x :t fixnum 10)   ; x is 10
         (y :t string)      ; y is an empty vector of characters
         (z (random 42))    ; z is exactly what it is supposed to be
         ((a :t fixnum b) (floor 179 57)))  ; a and b are 3 and 8 respectively
  body)"
    (labels ((rec (bindings)
               (if bindings
                   (destructuring-bind (bind . rest) bindings
                     (assert (and (listp bind) (<= (length bind) 4)))
                     (destructuring-bind (names . types-and-values) bind
                       (if (symbolp names)
                           (let ((typedec (member :t types-and-values)))
                             (if typedec
                                 (if (null (cdr typedec))
                                     (setf types-and-values `(:t t :t)) ;;TODO ugly way of biding :t specifically
                                     (setf types-and-values (third typedec)))
                                 (setf types-and-values (first types-and-values)))
                             `(let ((,names ,(or types-and-values
                                                (if typedec (default (second typedec))))))
                                (declare . ,(if typedec `((type ,(second typedec) ,names))))
                                ,(rec rest)))
                           (let ((types) (actual-names))
                             (labels ((get-types (ls)
                                        (if (< 2 (length ls))
                                            (destructuring-bind (name t? type? . rest) ls
                                              (if (eql :t t?)
                                                  (progn (push type? types)
                                                         (push name actual-names)
                                                         (get-types rest))
                                                  (progn (push '_ types)
                                                         (push name actual-names)
                                                         (get-types (cons t? (cons type? rest))))))
                                            (loop :for last :in ls
                                                  :do (push '_ types)
                                                      (push last actual-names)))))
                               (get-types names)
                               (setf types (nreverse types))
                               (setf actual-names (nreverse actual-names))
                               `(multiple-value-bind ,actual-names ,@types-and-values
                                  (declare . ,(if types
                                                  (loop :for name :in actual-names
                                                        :for type :in types
                                                        :unless (eql '_ type)
                                                          :collect `(type ,type ,name))))
                                  ,(rec rest)))))))
                   `(locally ,@body))))
      (rec bindings))))


;;FIXME does it belong here?
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *struct-name* (make-hash-table :test #'equalp)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun encode-type (obj-name &rest typenames)
    (or (gethash (cons obj-name typenames) *struct-name*)
       (setf (gethash (cons obj-name typenames) *struct-name*)
             (gentemp)))))

;;TODO the idea is good, but don't know if its usable like this
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-struct (name inheritance &body slots)
    (let ((trueslots (loop :for (name . rest) :in slots
                           :collect (ecase (length rest)
                                      (1 `(,name . ,rest))
                                      (2 (let ((type (cadr (member :t rest))))
                                           `(,name ,(default type) :type ,type)))
                                      (3 (let ((type (cadr (member :t rest))))
                                           `(,name ,(third rest) :type ,type)))))))

      `(progn
         (defstruct ,(if inheritance
                         `(,name (:include ,inheritance))
                         name)
           ,@trueslots)
         ,(unless (fboundp name)
            `(define-polymorphic-function ,name (&optional
                                                 ,@(loop :for (name . rest) :in slots
                                                         :collect name))
               :overwrite t))
         (defpolymorph (,name :inline t)
             (&optional ,@(loop :for (sname . rest) :in slots
                                :collect (ecase (length rest)
                                           (1 `((,sname t) . ,rest))
                                           (2 (let ((type (cadr (member :t rest))))
                                                `((,sname ,type) ,(default type))))
                                           (3 (let ((type (cadr (member :t rest))))
                                                `((,sname ,type) ,(third rest)))))))
             ,name
           (,(intern (concatenate 'string "MAKE-" (string name)))
            ,@(loop :for (sname . _) :in slots
                    :appending `(,(intern (string sname) "KEYWORD") ,sname))))
         ,@(loop :for (sname . rest) :in slots
                 :for type := (ecase (length rest)
                                (1 t)
                                ((2 3) (cadr (member :t rest))))
                 :collect `(progn
                             ,(unless (fboundp sname)
                                `(define-polymorphic-function ,sname (object) :overwrite t))
                             (defpolymorph (,sname :inline t)
                                 ((object ,name)) ,type
                               (,(intern (concatenate 'string (string name) "-" (string sname)))
                                object))
                             ,(unless (fboundp `(setf ,sname))
                                `(define-polymorphic-function (setf ,sname) (new object) :overwrite t))
                             (defpolymorph ((setf,sname) :inline t)
                                 ((new ,type) (object ,name)) ,type
                               (setf (,(intern (concatenate 'string (string name) "-" (string sname)))
                                      object)
                                     new))))))))


;;FIXME doesn't work properly
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-t-slots (names object &body body)
    `(symbol-macrolet ,(loop :for name :in names
                             :collect (if (listp name)
                                          (destructuring-bind (vname key type &optional (newname vname)) name
                                            (assert (member key '(:t :type)))
                                            `(,newname (the ,type (,vname ,object))))
                                          `(,name (,name ,object))))
       ,@body)))

;;TODO redo for interfaces
(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype has-binops (&rest functions)
    (let ((intersec))
      (loop :for fn :in functions
            :for lists := (mapcar #'polymorphic-functions::polymorph-type-list
                                  (polymorphic-functions::polymorphic-function-polymorphs
                                   (fdefinition fn)))
            :for res := (loop :for list :in lists
                              :when (and (= 2 (length list))
                                       (eql (first list) (second list)))
                                :collect (first list))
            :do (setf intersec (if intersec (intersection res intersec) res)))
      `(or ,@intersec))))

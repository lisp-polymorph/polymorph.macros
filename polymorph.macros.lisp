;;;; polymorph.macros.lisp

(in-package #:polymorph.macros)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro zapf (place name expr &environment env)
    "Usage: setting an element of a container in an efficient way.
  Name argument refers to the name that can be used in an expr instead of a place value.
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

(defmacro %setf (place val &environment env)
  "Usage: setting a place similar to setf, but in a type-safe way."
  (if (symbolp place)
      (if (subtypep (%form-type val env) (%form-type place env) env)
          `(setq ,place ,val)
          (error 'type-error :context (format nil "when SETFing ~s as ~s"
                                              place val)
                             :expected-type (%form-type place env) :datum val))
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

(defmacro setf* (&rest args)
  (assert (evenp (length args)) (args) "Odd number of arguments to SETF")
  `(progn ,@(loop :for (place val) :on args :by #'cddr
                  :collect `(%setf ,place ,val))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro bind* (bindings &body body) ;; Replacement for let*
    "Bind* unites 3 things: let*, multiple-value-bind and builtin type declarations.
   Uses default for filling out the values if types was provided, otherwise defaults to nil.
Examples of usage:

(bind* ((x :t fixnum 10)   ; x is 10
        (y :t string)      ; y is an empty vector of characters
        (z (random 42))    ; z is a random number
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
                                                  (progn (push t types)
                                                         (push name actual-names)
                                                         (get-types (cons t? (cons type? rest))))))
                                            (loop :for last :in ls
                                                  :do (push t types)
                                                      (push last actual-names)))))
                               (get-types names)
                               (setf types (nreverse types))
                               (setf actual-names (nreverse actual-names))
                               `(multiple-value-bind ,actual-names ,@types-and-values
                                  (declare . ,(if types
                                                  (loop :for name :in actual-names
                                                        :for type :in types
                                                        :collect `(type ,type ,name))))
                                  ,(rec rest)))))))
                   `(locally ,@body))))
      (rec bindings))))



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro bind (bindings &body body) ;; Replacement for let
    (let ((names) (types) (forms))
      (loop :for bind :in bindings
            :do (destructuring-bind (name &rest stuff) bind
                  (if (listp name)
                      (labels ((get-types (ls)
                                 (if (< 2 (length ls))
                                     (destructuring-bind (name t? type? . rest) ls
                                       (if (eql :t t?)
                                           (progn (push type? types)
                                                  (push name names)
                                                  (get-types rest))
                                           (progn (push t types)
                                                  (push name names)
                                                  (get-types (cons t? (cons type? rest))))))
                                     (loop :for last :in ls
                                           :do (push t types)
                                               (push last names)))))
                        (get-types name)
                        (push (first stuff) forms))
                      (destructuring-bind
                          (&optional sign (type 'null) (form (default type))) stuff
                        (push name names)
                        (cond ((eq :t sign)
                               (push type types)
                               (push form forms))
                              ((= 0 (length stuff))
                               (push t types)
                               (push nil forms))
                              (t
                               (push t types) ;; TODO this is where some type inference can be done
                               (push sign forms)))))))
      (setf names (nreverse names)
            types (nreverse types)
            forms (nreverse forms))
      `(multiple-value-call
           (lambda ,names
             (declare ,@(loop :for name :in names :for type :in types
                              :collect `(type ,type ,name)))
             (progn ,@body))
         ,@forms))))


 


#||
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



;;TODO redo for interfaces
;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (deftype has-binops (&rest functions)
;;    (let ((intersec))
;;      (loop :for fn :in functions
;;            :for lists := (mapcar #'polymorphic-functions::polymorph-type-list
;;                              :when (and (= 2 (length list)))
;;                                  (polymorphic-functions::polymorphic-function-polymorphs
;;                                   (fdefinition fn)
;;            :for res := (loop :for list :in lists
;;                                       (eql (first list) (second list))
;;                                :collect (first list)
;;            :do (setf intersec (if intersec (intersection res intersec) res))
;;      `(or ,@intersec))))

||#

;; TODO This is best one so far
(defmacro def (name (&rest traits) &body slots)
  (let ((typed-slots
          (loop :for slot :in slots
                :collect (if (listp slot)
                             (if (eq (first slot) :mut)
                                 (destructuring-bind
                                     (mut sname &optional (stype t) (sform (default stype)))
                                     slot
                                   (declare (ignore mut))
                                   `(,sname ,stype ,sform nil))
                                 (destructuring-bind
                                     (sname &optional (stype t) (sform (default stype)))
                                     slot
                                   `(,sname ,stype ,sform t)))
                             `(,slot t nil t)))))
    `(progn
       (defstruct ,name
         ,@(loop :for (sname stype sform const) :in typed-slots
                 :collect `(,sname ,sform
                                   :type ,stype
                                   :read-only ,const)))
       ,(unless (fboundp name)
          `(define-polymorphic-function ,name (&key ,@(loop :for (sname) :in typed-slots
                                                            :collect sname))))
       (defpolymorph (,name :inline t) (&key ,@(loop :for (sname stype sform) :in typed-slots
                                                     :collect (list (list sname stype) sform)))
           ,name
         (,(intern (format nil "MAKE-~s" name))
          ,@(loop :for (sname) :in typed-slots
                  :appending (list (intern (string sname) "KEYWORD") sname))))
       ,@(loop :for (sname stype _ const) :in typed-slots
               :unless (fboundp sname)
                 :collect `(define-polymorphic-function ,sname (object) :overwrite t)
               :collect `(defpolymorph (,sname :inline t) ((,name ,name)) (values ,stype &optional)
                           (,(intern (format nil "~s-~s" name sname))
                            ,name))
               :unless const
                 :unless (fboundp `(cl:setf ,sname))
                   :collect `(define-polymorphic-function (cl:setf ,sname) (new object) :overwrite t)
               :unless const :collect `(defpolymorph ((cl:setf ,sname) :inline t) ((new ,stype) (,name ,name)) (values ,stype &optional)
                                         (cl:setf (,(intern (format nil "~s-~s" name sname))
                                                   ,name)
                                                  new)))
       ,(when (member :eq traits)
          `(defpolymorph (= :inline t) ((first ,name) (second ,name)) boolean
             (and ,@(loop :for (sname) :in typed-slots
                          :collect `(= (,sname first) (,sname second))))))
       ,(when (member :copy traits)
          `(progn
             (defpolymorph (deep-copy :inline t) ((object ,name)) ,name
               (,(intern (format nil "MAKE-~s" name))
                ,@(loop :for (sname) :in typed-slots
                        :appending `(,(intern (string sname) "KEYWORD") (deep-copy (,sname object))))))
             (defpolymorph (shallow-copy :inline t) ((object ,name)) ,name
               (,(intern (format nil "MAKE-~s" name))
                ,@(loop :for (sname) :in typed-slots
                        :appending `(,(intern (string sname) "KEYWORD") (,sname object)))))))
       ',name)))

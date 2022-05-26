;;;; polymorph.macros.lisp

(in-package #:polymorph.macros)


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
      ,store-expr)))

(defmacro %setf (place val &environment env)
  (if (symbolp place)
      (if (subtypep (%form-type val env) (%form-type place env) env)
          `(setq ,place (the ,(%form-type val env) ,val))
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
  "Usage: setting a place similar to setf, but in a type-safe way."
  (assert (evenp (length args)) (args) "Odd number of arguments to SETF")
  `(progn ,@(loop :for (place val) :on args :by #'cddr
                  :collect `(%setf ,place ,val))))


(defmacro bind* (bindings &body body &environment env)
  "Bind* unites 3 things: let*, multiple-value-bind and builtin type declarations.
   Uses default for filling out the values if types was provided, otherwise defaults to nil.
  :infer can used to tell the macro to declare the variable to be of inferred type.
Examples of usage:

(bind* (((x fixnum) 10)   ; x is 10
        ((y string))      ; y is an empty vector of characters
        (z (random 42))    ; z is a random number
        ((a fixnum) b (floor 179 57)))  ; a and b are 3 and 8 respectively
  body)"
  (labels ((rec (ls)
             (if ls
                 (destructuring-bind (bind . rest) ls
                   (cond
                     ((= 1 (length bind))
                      (let ((name (first bind)))
                        (if (symbolp name)
                            `(let ((,name))
                               ,(rec rest))
                            (destructuring-bind (name type) name
                              (if (eql type :infer)
                                 `(let ((,name))
                                    ,(rec rest))
                                 `(let ((,name ,(default type)))
                                    (declare (type ,type ,name))
                                    ,(rec rest)))))))
                     ((= 2 (length bind))
                      (destructuring-bind (name val) bind
                        (if (symbolp name)
                            `(let ((,name ,val))
                               ,(rec rest))
                            (destructuring-bind (name type) name
                              (if (eql type :infer)
                                  (let ((type (%form-type val env)))
                                    `(let ((,name ,val))
                                       (declare (type ,type ,name))
                                       ,(rec rest)))
                                  `(let ((,name ,val))
                                     (declare (type ,type ,name))
                                     ,(rec rest)))))))
                     ((< 2 (length bind))
                      (let* ((names (butlast bind))
                             (val (first (last bind)))
                             (inftypes (%list-form-types val env))
                             (types (loop :for name :in names
                                          :for i :from 0
                                          :unless (symbolp name)
                                            :collect (cons (second name) i))))
                        `(multiple-value-bind ,(mapcar (lambda (x) (if (symbolp x) x (first x))) names)
                             ,val
                           (declare ,@(when types
                                        (loop :for (type . pos) :in types
                                              :collect (if (eql type :infer)
                                                           `(type ,(or (nth pos inftypes) 'null)
                                                                  ,(first (elt names pos)))
                                                           `(type ,type ,(first (elt names pos)))))))
                           ,(rec rest))))
                     (t (error "Invalid syntax in BIND*"))))
                 `(locally ,@body))))
    (rec bindings)))


(defmacro bind (bindings &body body &environment env)
    "Bind unites 3 things: let, multiple-value-bind and builtin type declarations.
   Uses default for filling out the values if types was provided, otherwise defaults to nil.
Examples of usage:

(bind (((x fixnum) 10)   ; x is 10
        ((y string))      ; y is an empty vector of characters
        (z (random 42))    ; z is a random number
        ((a fixnum) b (floor 179 57)))  ; a and b are 3 and 8 respectively
  body)"

  (let ((names) (names-and-types) (vals))
    (loop :for bind :in bindings
          :do (if (= 1 (length bind))
                  (let ((name (first bind)))
                    (if (symbolp name)
                        (progn (push name names)
                               (push nil vals))
                        (destructuring-bind (name type) name
                          (if (eql type :infer)
                              (progn
                                (push name names)
                                (push (cons t name) names-and-types)
                                (push nil vals))
                              (progn
                                (push name names)
                                (push (cons type name) names-and-types)
                                (push (default type) vals))))))
                  (let* ((val (first (last bind)))
                         (inftypes (%list-form-types val env)))
                   (loop :for name :in (butlast bind)
                         :for i :from 0
                         :if (symbolp name)
                           :do (push name names)
                         :else :do (destructuring-bind (name type) name
                                     (if (eql type :infer)
                                         (progn
                                           (push name names)
                                           (push (cons (or (nth i inftypes) 'null)
                                                       name)
                                                 names-and-types))
                                         (progn
                                           (push name names)
                                           (push (cons type name) names-and-types)))))
                   (push val vals))))
    (setf names (reverse names)
          names-and-types (reverse names-and-types)
          vals (reverse vals))
    `(multiple-value-call
         (lambda ,names
           (declare ,@(loop :for (type . name) :in names-and-types
                            :collect `(type ,type ,name)))
           ,@body)
       ,@vals)))



   


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


;; TODO This is best one so far
(defmacro def (name (&rest traits) &body slots)
  "Defines a structure with polymorphic constructor and accessors. :mut is for declaring slots
mutable. Default values are filled according to types. Doesn't have syntax for inheritance.
You can provide :eq and :copy as indicators that there should also be = and copy defined.
Example of usage:
  (def user (:eq :copy)
   (name simple-string)
   (:mut age (integer 0 200)))"
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
;                           (defpolymorph-compiler-macro ,sname (,name) (,name)
 ;                            `(the ,',stype (,',(intern (format nil "~s-~s" name sname))
  ;                                           ,,name)))
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


;; Experimental area
(defmacro case= (expr &body forms)
  (let ((res (gensym "RESULT")))
    `(let ((,res ,expr))
       (cond ,@(loop :for (expected actions) :in forms
                     :collect (if (atom expected)
                                  `((polymorph.maths:= ,res ,expected)
                                    ,actions)
                                  `((or ,@(loop :for ex :in expected
                                                :collect `(polymorph.maths:= ,res ,ex)))
                                    ,actions)))))))



(defun %parse-typed-lambda-list (ls)
  (let ((names) (types) (argtype '&req))
    (assert (>= 1 (count '&optional ls)))
    (assert (>= 1 (count '&key ls)))
    (assert (>= 1 (count '&rest ls)))
    (assert (>= 1 (count '&aux ls)))
    (labels ((rec (list)
               (when list
                 (destructuring-bind (head . tail) list
                   (if (atom head)
                       (if (member head '(&optional &key &aux &rest))
                           (setf argtype head)
                           (unless (eql argtype '&rest)
                             (push head names)
                             (push t types)))
                       (if (atom (first head))
                           (destructuring-bind (name type) head
                             (assert (symbolp name))
                             (push name names)
                             (push type types))
                           (destructuring-bind (name type) (first head)
                             (assert (symbolp name))
                             (push name names)
                             (push type types))))
                   (rec tail)))))
      (rec ls)
      (values (reverse names) (reverse types)))))



(defmacro lambda* (typed-lambda-list return-type &body body)
  (multiple-value-bind (names types) (%parse-typed-lambda-list typed-lambda-list)
    `(lambda ,names
       (declare ,@(loop :for name :in names
                        :for type :in types
                        :collect `(type ,type ,name))
                ,(if (atom return-type)
                     `(values ,return-type &optional)
                     return-type))
       ,@body)))


(defmacro while (condition &body body)
  (let ((start (gensym "START")))
    `(block nil
       (tagbody
          ,start
          (when ,condition
            ,@body
            (go ,start))))))

(defmacro until (condition &body body)
  (let ((start (gensym "START")))
    `(block nil
       (tagbody
          ,start
          (unless ,condition
            ,@body
            (go ,start))))))

#||
(defpolymorph add-into ((a number) (b number) (c number)) number
  (declare (ignorable c))
  (polymorph.maths:+ a b))


(defmacro += (place val &environment env)
  (multiple-value-bind
       (temps exprs stores store-expr access-expr)
     (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
              (add-into ,access-expr ,val ,access-expr)))
       ,store-expr)))

(defpolymorph substract-into ((a number) (b number) (c number)) number
  (declare (ignorable c))
  (polymorph.maths:- a b))

(defmacro -= (place val &environment env)
  (multiple-value-bind
       (temps exprs stores store-expr access-expr)
     (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
              (substract-into ,access-expr ,val ,access-expr)))
       ,store-expr)))

(defpolymorph multiply-into ((a number) (b number) (c number)) number
  (declare (ignorable c))
  (polymorph.maths:* a b))

(defmacro *= (place val &environment env)
  (multiple-value-bind
       (temps exprs stores store-expr access-expr)
     (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
              (multiply-into ,access-expr ,val ,access-expr)))
       ,store-expr)))

(defpolymorph divide-into ((a number) (b number) (c number)) number
  (declare (ignorable c))
  (polymorph.maths:/ a b))

(defmacro /= (place val &environment env)
  (multiple-value-bind
       (temps exprs stores store-expr access-expr)
     (get-setf-expansion place env)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
              (divide-into ,access-expr ,val ,access-expr)))
       ,store-expr)))
||#

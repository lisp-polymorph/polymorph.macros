;;;; polymorph.utility.lisp

(in-package #:polymorph.utility)


(deftype ind () `(integer 0 #.array-dimension-limit))

(defparameter *default-impl* (make-hash-table))



(defun %dimensions-comp (dimensions)
  (cond ((eql '* dimensions) 0)
        ((listp dimensions) (mapcar (lambda (x) (if (eql '* x) 0 x)) dimensions))
        (t dimensions)))


(defun default (type &optional environment)
  "Return a reasonable default object for a given type."
  (multiple-value-bind (item knownp) (gethash type *default-impl*)
    (if knownp
        item
        (progn
          (setf type (sb-ext:typexpand type environment))
          (if (symbolp type)
              (case type
                ((bit fixnum integer rational) 0)
                ((float double-float single-float long-float real) 0.0)
                ((number complex) #c(0 0))
                ((character base-char) #\Nul)
                (standard-char #\a)
                ((symbol t) t)
                (keyword :t)
                (hash-table `(make-hash-table))
                ((list boolean atom null) nil)
                (pathname #P"")
                (function '(lambda (&rest args)
                            (declare (ignore args)
                             (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))))
                (vector '(make-array 0 :adjustable t))
                (bit-vector '(make-array 0 :element-type 'bit :adjustable t))
                (string '(make-array 0 :element-type 'character :adjustable t :initial-element #\Nul))
                (simple-array (make-array 0)) ;;Maybe it should error here, since array dimension is nto specified?
                ;;What happens with just array? Or just sequence? I guess nothing
                (simple-string '(make-array 0 :element-type 'character :initial-element #\Nul))
                (simple-base-string '(make-array 0 :element-type 'base-char :initial-element #\Nul))
                (otherwise
                 (cond ((subtypep type 'structure-object environment)
                        (list (intern (concatenate 'string "MAKE-" (string type)))))
                       ((subtypep type 'standard-object environment)
                        `(make-instance ,type)))))
              (destructuring-bind (main . rest) type
                (case main
                  ((mod unsigned-byte singned-byte) 0)
                  ((integer eql member rational real float) (first rest))
                  (complex `(complex ,(default (first rest)) ,(default (first rest))))
                  (cons `(cons ,(default (first rest)) ,(default (first rest))))
                  (or (default (first rest)))
                  (vector `(make-array ',(if (= 2 (length rest))
                                             (%dimensions-comp (second rest))
                                             0)
                                       :adjustable t
                                       :element-type ',(or (first rest) t)
                                       :initial-element ,(if (first rest)
                                                             (default (first rest))
                                                             0)))
                  (bit-vector `(make-array ,(or (first rest) 0) :element-type 'bit :adjustable t))
                  (string `(make-array ',(if (= 2 (length rest))
                                             (%dimensions-comp (second rest))
                                             0)
                                       :element-type 'character
                                       :adjustable t
                                       :initial-element #\Nul))
                  (simple-array `(make-array ',(if (= 2 (length rest))
                                                   (%dimensions-comp (second rest))
                                                   0)
                                             :element-type ',(or (first rest) t)
                                             :initial-element ,(if (first rest)
                                                                   (default (first rest))
                                                                   0)))
                  (simple-string `(make-array ',(if (= 2 (length rest))
                                                    (%dimensions-comp (second rest))
                                                    0)
                                              :element-type 'character
                                              :initial-element #\Nul))
                  (simple-base-string `(make-array ',(if (= 2 (length rest))
                                                         (%dimensions-comp (second rest))
                                                         0)
                                                   :element-type 'base-char
                                                   :initial-element #\Nul))

                  (array `(make-array ',(if (= 2 (length rest))
                                            (%dimensions-comp (second rest))
                                            0)
                                      :element-type ',(or (first rest) t)
                                      :initial-element ,(if (first rest)
                                                            (default (first rest))
                                                            0))))))))))





(defmacro zapf ((place name) expr)
  "Usage: setting element of a container in an efficient way.
Name argument refers to the name that can be used in an expr instead of a palce value.
  Example: (zapf ((gethash ht key) v) (+ v (expt v v)))"
  (multiple-value-bind
        (temps exprs stores store-expr access-expr)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list temps exprs)
            (,(car stores)
             (let ((,name ,access-expr))
               ,expr)))
       ,store-expr)))




(defmacro tbind* (bindings &body body)
  "Bind unites 3 things: let*, multiple-value-bind and builtin type declarations.
   Uses default for filling out the values if types was provided, otherwise defaults to nil.
Examples of usage:

(tbind* ((x :t fixnum 10)   ; x is 10
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
                 `(progn ,@body))))
    (rec bindings)))

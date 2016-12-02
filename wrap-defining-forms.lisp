;;;; Wrap--Defining-Forms
;;; -*- Lisp -*-

(defpackage wrap-defining-forms
  (:use :common-lisp)
  (:documentation "Wrap  defining forms so  that they (try to)  save the
  source code of the definition being passed.")
  (:export #:wrap-defining-form #:dump-definitions
           
           #:defclass$
           #:defconstant$
           #:defgeneric$
           #:define-compiler-macro$
           #:define-condition$
           #:define-method-combination$
           #:define-modify-macro$
           #:define-setf-expander$
           #:define-symbol-macro$
           #:defmacro$
           #:defmethod$
           #:defpackage$
           #:defparameter$
           #:defsetf$
           #:defstruct$
           #:deftype$
           #:defun$
           #:defvar$))

(defpackage wrap-defining-forms.shadowing
  (:documentation "Wrapped forms like DEFUN$  are exported here with the
  names   of   the    forms   that   they   wrap,    like   DEFUN,   for
  shadowing imports.")
  (:export #:defclass
           #:defconstant
           #:defgeneric
           #:define-compiler-macro
           #:define-condition
           #:define-method-combination
           #:define-modify-macro
           #:define-setf-expander
           #:define-symbol-macro
           #:defmacro
           #:defmethod
           #:defpackage
           #:defparameter
           #:defsetf
           #:defstruct
           #:deftype
           #:defun
           #:defvar)
  (:use))

;; Clozure appears  to be  “smart” and adds  Common-Lisp even  though we
;; didn't ask for it (and explicily don't want it)
#+ccl (unuse-package '(:ccl :common-lisp)
                     :wrap-defining-forms.shadowing)

(defpackage :common-lisp-user/save-defs
  (:nicknames :cl-user$)
  (:use :common-lisp :common-lisp-user)
  (:import-from :wrap-defining-forms #:dump-definitions)
  (:shadowing-import-from :wrap-defining-forms.shadowing
                          #:defclass
                          #:defconstant
                          #:defgeneric
                          #:define-compiler-macro
                          #:define-condition
                          #:define-method-combination
                          #:define-modify-macro
                          #:define-setf-expander
                          #:define-symbol-macro
                          #:defmacro
                          #:defmethod
                          #:defpackage
                          #:defparameter
                          #:defsetf
                          #:defstruct
                          #:deftype
                          #:defun
                          #:defvar))
;; Clone any other functions you may have packed into CL-User.
(with-package-iterator (next-symbol :common-lisp-user :internal)
  (loop for symbol = (next-symbol)
        for sibling = (when symbol
                        (intern (symbol-name symbol) (find-package :cl-user$)))
        while symbol
        when (and (fboundp symbol)
                  (not (fboundp sibling)))
          do (setf (fdefinition sibling) (fdefinition symbol))))
(in-package "WRAP-DEFINING-FORMS")

(defvar *definitions* (make-hash-table)
  "Copies   of    forms   defined    by   the   wrappers    created   by
  `WRAP-DEFINING-FORM' which can be stashed with `DUMP-DEFINITIONS'")

(defun mock-lambda-list (required optional restp keywords)
  (concatenate ' list
                 (loop repeat required 
                       collect (gensym "ARG-"))
                 (when (and optional (plusp optional))
                   (cons '&optional
                         (loop repeat optional
                               collect (gensym "OPT-"))))
                 (when restp
                   (list '&rest 'rest))
                 (when (and keywords (plusp keywords))
                   (list '&key '&allow-other-keys))))

#+ccl
(defun ccl-mock-lambda-list (function)
  (if (macro-function function)
      (make-unknown-lambda-list function)
      (multiple-value-bind (required optional restp
                            keywords) 
          (ccl:function-args (fdefinition function))
        (mock-lambda-list required optional restp keywords))))

(defun make-unknown-lambda-list (function)
  (declare (ignore function))
  (list '&rest 'unknown-lambda-list))

(defun find-function-lambda-list ()
  "Find the implementation's version  of `FUNCTION-LAMBDA-LIST' if there
is  one.  That  way,  Slime  and  friends  can  still  give  the  proper
lambda-list  for the  wrapped form.  If it  can't be  found, this  will
return a stub with just a &rest-var."
  (or
   #+sbcl #'sb-introspect:function-lambda-list
   #+ccl #'ccl-mock-lambda-list
   #+clisp #'system::arglist
   #+allegro #'excl::arglist
   #+lispworks #'lw:function-lambda-list ; I have not tested this
   #+ecl #'ext:function-lambda-list
   #-(or ccl sbcl clisp allegro lispworks)
   (dolist (package (list-all-packages))
     (let ((sym (or (find-symbol "FUNCTION-LAMBDA-LIST" package)
                    (find-symbol "ARGLIST" package))))
       (when (fboundp sym)
         (warn "Guessing that ~a function ~s is like FUNCTION-LAMBDA-LIST from SBCL"
               (lisp-implementation-type) sym)
         (return-from find-function-lambda-list sym)))) 
   #'make-unknown-lambda-list))

(defmacro wrap-defining-form (cl-form) 
  "Assuming  that CL-FORM  is a  symbol for  a macro  or function  which
defines something  interesting (eg, “Defun”),  this will create  a macro
with the same  name with a trailing  “$” that will save  the source tree
before passing on the form to CL-FORM.

EG:  (wrap-defining-form  defun)  provides  a  “defun$”  which  has  the
additional side effect of storing the source form in *DEFINITIONS*.

Definitions saved can be recovered by `DUMP-DEFINITIONS'.

This  is not  industrial-strength; in  particular, I  expect it  to cope
poorly with DEFMETHOD."
  (check-type cl-form symbol)
  (let ((wrapper (intern (concatenate 'string (symbol-name cl-form) "$")))
        (wrapper.shadow (intern (symbol-name cl-form) :wrap-defining-forms.shadowing))
        (wrapped-lambda-list (funcall (find-function-lambda-list) 'defun)))
    (setf (gethash cl-form *definitions*) (make-hash-table))
    `(prog1
         (defmacro ,wrapper (&whole whole ,@wrapped-lambda-list)
           (declare (ignore ,@(remove-if (lambda (form) (member form lambda-list-keywords))
                                         wrapped-lambda-list)))
           ,(concatenate 'string "Wrap `" (symbol-name cl-form) "' and save the original form." #(#\newline #\newline)
                         (symbol-name cl-form) ": " (or (documentation cl-form 'function)
                                                        "(see CLHS; no documentation here)"))
           (let ((defined (cons ',cl-form (cdr whole))))
             (setf (gethash (second whole) (gethash ',cl-form *definitions*))
                   defined)
             defined))
       (defmacro ,wrapper.shadow (&whole whole ,@wrapped-lambda-list)
         (declare (ignore ,@(remove-if (lambda (form) (member form lambda-list-keywords))
                                       wrapped-lambda-list)))
         ,(concatenate 'string "Wrap `COMMON-LISP:" (symbol-name cl-form) "' and save the original form."
                       #(#\newline #\newline)
                       (symbol-name cl-form) ": " (or (documentation cl-form 'function)
                                                      "(see CLHS; no documentation here)"))
         (let ((defined (cons ',cl-form (cdr whole))))
           (setf (gethash (second whole) (gethash ',cl-form *definitions*)) 
                 defined)
           defined)))))
(wrap-defining-form defclass)
(wrap-defining-form defconstant)
(wrap-defining-form defgeneric)
(wrap-defining-form define-compiler-macro)
(wrap-defining-form define-condition)
(wrap-defining-form define-method-combination)
(wrap-defining-form define-modify-macro)
(wrap-defining-form define-setf-expander)
(wrap-defining-form define-symbol-macro)
(wrap-defining-form defmacro)
(wrap-defining-form defmethod)
(wrap-defining-form defpackage)
(wrap-defining-form defparameter)
(wrap-defining-form defsetf)
(wrap-defining-form defstruct)
(wrap-defining-form deftype)
(wrap-defining-form defun)
(wrap-defining-form defvar)
(defun dump-definitions (&optional pathname)
  "Write  out   the  definitions  saved   by  `WRAP-DEFINING-FORM'-built
wrappers to PATHNAME (or *STANDARD-OUTPUT*)."
  (let (output
        (*print-case* :capitalize)
        ;; If writing to file, set margin at 79, but try to keep things under 72.
        (*print-right-margin* (if pathname 79 *print-right-margin*))
        (*print-miser-width* (if pathname 72 *print-miser-width*)))
    (unwind-protect
         (progn (setq output (if pathname
                                 (open pathname :direction :output
                                                :if-exists :rename
                                                :if-does-not-exist :create)
                                 *standard-output*))
                (multiple-value-bind  (sec min hr d m y) (decode-universal-time (get-universal-time))
                  (declare (ignore sec))
                  (format output
                          "~&~|~%;;; definitions as of ~d-~d-~d @ ~d:~2,'0d:
\(In-Package #:~a)
~{~{~2%~:<~W ~@_~:I~W ~:_~W~1I ~_~W~:>~}~^~|~}~%~|~%" ; from CLHS 22.2.2 SIMPLE-PPRINT-DEFUN
                          y m d hr min
                          (package-name *package*)
                          (remove-if #'null
                                     (loop for form being the hash-keys of *definitions*
                                           for defs = (gethash form *definitions*)
                                           collect (loop for definition being the hash-values of defs
                                                         collect definition))))))
      (when output (ignore-errors (close output))))))


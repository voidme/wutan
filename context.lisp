;;; context.lisp
;; Common Lisp translation of wutan/context.py
;; Provides a simple Context struct with parent chaining and handler functions.

(defpackage :wutan-context
  (:use :cl)
  (:export :context
           :make-context
           :context-get
           :context-set
           :context-is-extended-p
           :context-extend
           :context-handles))

(in-package :wutan-context)

(defstruct (context (:constructor make-context-struct (&optional data parent)))
  (data (make-hash-table :test 'equal))
  parent)

;; Provide a friendly positional constructor that calls the structure constructor.
(defun make-context (&optional data parent)
  "Create a new context. DATA should be a hash-table or nil.
This wrapper calls the underlying structure constructor created above." 
  (let ((tbl (or data (make-hash-table :test 'equal))))
    (make-context-struct tbl parent)))

(defun context-get (ctx key &optional (default nil) (source nil))
  "Get KEY from CTX. If stored value is a function, call it with SOURCE (or CTX).
If value is nil and CTX has a parent, delegate to parent. If not found, return DEFAULT."
  (let ((source (or source ctx)))
    (multiple-value-bind (value present) (gethash key (context-data ctx))
      (cond
        ((and present (functionp value))
         (funcall value source))
        ((and present (null value) (context-parent ctx))
         (context-get (context-parent ctx) key default source))
        ((not present)
         (if (context-parent ctx)
             (context-get (context-parent ctx) key default source)
             default))
        ((and present (null value))
         ;; present but nil, no parent
         default)
        (t value)))))

(defun context-set (ctx key value)
  "Set KEY to VALUE in CTX." 
  (setf (gethash key (context-data ctx)) value)
  ctx)

(defun context-is-extended-p (ctx other)
  "Return T if OTHER is CTX or is in CTX's parent chain." 
  (or (eq ctx other)
      (and (context-parent ctx)
           (context-is-extended-p (context-parent ctx) other))))

(defun context-extend (ctx other)
  "Insert OTHER into CTX's parent chain unless already extended."
  (unless (context-is-extended-p ctx other)
    (let ((extended (context-parent ctx)))
      (setf (context-parent ctx) other)
      (setf (context-parent other) extended)))
  ctx)

(defun context-handles (ctx key fn)
  "Register FN as a handler for KEY in CTX. FN should accept one argument (the source context)."
  (context-set ctx key fn)
  ctx)

;; Small self-test helper
(defun context--test ()
  (let* ((context1 (make-context))
         (context2 (make-context nil context1))
         (context3 (make-context nil context2)))
    (context-set context1 "a" 1)
    (context-set context2 "b" 2)
    (context-set context3 "c" 3)
    (context-handles context1 "test" (lambda (ctx) (1+ (context-get ctx "a"))))
    (assert (= (context-get context3 "a") 1))
    (assert (= (context-get context3 "b") 2))
    (assert (= (context-get context3 "c") 3))
    (assert (= (context-get context3 "d" 4) 4))
    (assert (= (context-get context1 "test") 2))
    (values t)))

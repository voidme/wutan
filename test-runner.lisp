;;; test-runner.lisp
;;; Minimal, dependency-free test runner for wutan context and transformer

(format t "Loading modules...~%")
(load (merge-pathnames "context.lisp" #P"."))
(load (merge-pathnames "transformer.lisp" #P"."))
(format t "Modules loaded.~%~%")

(defvar *tests* '())
(defmacro deftest (name &body body)
  `(push (cons ',name (lambda () ,@body)) *tests*))

(defun assert-equal (expected actual &optional (msg nil))
  (unless (equal expected actual)
    (error "Assertion failed: ~a~% Expected: ~a~% Actual: ~a~%" (or msg "") expected actual)))

(defun run-tests ()
  (format t "Running ~a tests...~%" (length *tests*))
  (let ((passes 0) (fails 0))
    (dolist (entry (reverse *tests*))
      (let ((name (car entry)) (fn (cdr entry)))
        (format t "- ~a: " name)
        (handler-case
            (progn (funcall fn) (incf passes) (format t "PASS~%"))
          (error (e)
            (incf fails)
            (format t "FAIL~%  ~a~%" e)))))
    (format t "~%Summary: ~a passed, ~a failed~%" passes fails)
    (values passes fails)))

;; Tests for context
(deftest context-basic
  (let* ((c1 (wutan-context:make-context))
         (c2 (wutan-context:make-context nil c1))
         (c3 (wutan-context:make-context nil c2)))
    (wutan-context:context-set c1 "a" 1)
    (wutan-context:context-set c2 "b" 2)
    (wutan-context:context-set c3 "c" 3)
    (wutan-context:context-handles c1 "test" (lambda (ctx) (1+ (wutan-context:context-get ctx "a"))))
    (assert-equal 1 (wutan-context:context-get c3 "a"))
    (assert-equal 2 (wutan-context:context-get c3 "b"))
    (assert-equal 3 (wutan-context:context-get c3 "c"))
    (assert-equal 4 (wutan-context:context-get c3 "d" 4))
    (assert-equal 2 (wutan-context:context-get c1 "test"))))

(deftest context-extend
  (let* ((a (wutan-context:make-context)) (b (wutan-context:make-context nil a)) (c (wutan-context:make-context nil b)))
    (wutan-context:context-set a "x" 9)
    (wutan-context:context-set b "y" 8)
    (wutan-context:context-set c "z" 7)
    (assert-equal 9 (wutan-context:context-get c "x"))))

;; Tests for transformer
(setf wutan-transformer:*transformer-debug* t)

 (deftest transformer-basic
  (let ((ctx (wutan-context:make-context)))
    (wutan-context:context-set ctx "name" "Brian")
    (let ((content "Hello, {{name}}!"))
      (format t "DEBUG parse: ~a~%" (wutan-transformer:transformer-parse content))
      (format t "DEBUG join: ~a~%" (wutan-transformer:transformer-join ctx (wutan-transformer:transformer-parse content)))
      (assert-equal "Hello, Brian!" (wutan-transformer:transformer-transform ctx content)))))

(deftest transformer-unclosed
  (let ((ctx (wutan-context:make-context)))
    (assert-equal "Start unclosed" (wutan-transformer:transformer-transform ctx "Start {{unclosed"))))

(deftest transformer-multi
  (let ((ctx (wutan-context:make-context)))
    (wutan-context:context-set ctx "a" "one")
    (wutan-context:context-set ctx "b" "two")
    (assert-equal "one and two" (wutan-transformer:transformer-transform ctx "{{a}} and {{b}}"))))

;; Run and quit SBCL after tests
(multiple-value-bind (p f) (run-tests)
  (if (> f 0)
      (progn (format t "Some tests failed; exiting with code 1.~%") (sb-ext:quit 1))
      (progn (format t "All tests passed.~%") (sb-ext:quit 0))))

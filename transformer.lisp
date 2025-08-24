;;; transformer.lisp
;; Common Lisp translation of wutan/transformer.py
;; Simple template parser that finds {{var}} placeholders and replaces them with values from Context.

(defpackage :wutan-transformer
  (:use :cl :wutan-context)
  (:export :transformer-parse
           :transformer-join
           :transformer-transform
           :*transformer-debug*))
(in-package :wutan-transformer)

(defparameter *transformer-debug* nil "When true, print debug info from transformer-parse.")

(defun transformer-parse (content)
  "Parse CONTENT and return a list of (TEXT . VAR) pairs.
If VAR is NIL the TEXT is plain text. If VAR is non-nil the TEXT is the placeholder (including braces).
Behavior mirrors the original Python implementation." 
  (let* ((len (length content))
         (pos 0)
         (result '()))
    (loop while (< pos len) do
          (let ((start-pos (search "{{" content :start2 pos)))
            (when *transformer-debug* (format t "DEBUG pos=~a start-pos=~a~%" pos start-pos))
            (if (null start-pos)
                (progn
                  (when (< pos len)
                    (push (cons (subseq content pos len) nil) result))
                  (return (nreverse result)))
                ;; else: we found an opening marker
                (progn
                  (when (> start-pos pos)
                    (progn
                      (push (cons (subseq content pos start-pos) nil) result)
                      (when *transformer-debug* (format t "PUSH text=~a~%" (subseq content pos start-pos)))))
                  (let ((end-pos (search "}}" content :start2 (+ start-pos 2))))
                    (when *transformer-debug* (format t "DEBUG end-pos=~a~%" end-pos))
                    (if end-pos
                        (progn
                          (push (cons (subseq content start-pos (+ end-pos 2))
                                      (subseq content (+ start-pos 2) end-pos))
                                result)
                          (when *transformer-debug* (format t "PUSH placeholder=~a var=~a~%" (subseq content start-pos (+ end-pos 2)) (subseq content (+ start-pos 2) end-pos)))
                          (setf pos (+ end-pos 2)))
                        (progn
                          ;; no closing braces: treat remainder as plain text after the opening braces
                          (push (cons (subseq content (+ start-pos 2) len) nil) result)
                          (when *transformer-debug* (format t "PUSH unclosed=~a~%" (subseq content (+ start-pos 2) len)))
                          (setf pos (+ start-pos 2)))))))))
    (nreverse result)))

(defun transformer-join (ctx items)
  "Resolve ITEMS (list of (TEXT . VAR)) against CTX and return a list of string segments." 
  (mapcar (lambda (item)
            (let ((text (car item))
                  (var (cdr item)))
              (if var
                  (let ((val (wutan-context:context-get ctx var text)))
                    (format nil "~a" val))
                  text)))
          items))

;; Simple type check that is robust across packages: check for structure accessor function.
(defun transformer-transform (ctx content)
  "Transform CONTENT by replacing {{var}} placeholders using CTX." 
  ;; Check the concrete struct type exported by wutan-context package.
  (unless (typep ctx 'wutan-context:context)
    (error "context must be an instance of WUTAN-CONTEXT:CONTEXT (use make-context)"))
  ;; Simpler, robust implementation: scan content and write segments directly.
  (let ((len (length content)) (pos 0))
    (with-output-to-string (out)
      (loop while (< pos len) do
            (let ((start (search "{{" content :start2 pos)))
              (if (null start)
                  (progn (when (< pos len) (princ (subseq content pos len) out)) (setf pos len))
                  (progn
                    (when (> start pos) (princ (subseq content pos start) out))
                    (let ((end (search "}}" content :start2 (+ start 2))))
                      (if end
                          (let* ((var (subseq content (+ start 2) end))
                                 (val (wutan-context:context-get ctx var (subseq content start (+ end 2)))))
                            (princ (format nil "~a" val) out)
                            (setf pos (+ end 2)))
                          ;; no closing braces: append the remainder after the opening braces
                          (progn (princ (subseq content (+ start 2) len) out) (setf pos len)))))))
      out)))

;; small smoke test
(defun transformer--test ()
  (let* ((ctx1 (make-context))
         (ctx2 (make-context nil ctx1)))
    (context-set ctx1 "name" "Brian")
    (format t "~a~%" (transformer-transform ctx2 "Hello, {{name}}!"))
    (transformer-transform ctx2 "Hello, {{name}}!")))

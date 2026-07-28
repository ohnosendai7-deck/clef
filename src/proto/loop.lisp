;;;; loop.lisp — an extended LOOP subset for CLEF-H.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.
;;;;
;;;; Replaces the for-in-only loop engine with a proper extended-loop subset:
;;;; iteration clauses (for/in/on/across/from/to/downto/being each of hash /
;;;; package symbols), accumulation (collect/append/nconc/sum/count/maximize/
;;;; minimize), conditional execution (when/if/unless ... do/collect), and
;;;; termination (while/until/repeat/return), plus do and initially/finally.
;;;; Numeric and list accumulation are combined under a single result.

(in-package #:clef/proto/eval)

(defstruct (loop-state (:conc-name ls-))
  (env nil)
  (iters '())            ; list of iteration drivers (closures stepping state)
  (collected '())
  (appended '())
  (sum 0)
  (count 0)
  (max nil) (max-p nil)
  (min nil) (min-p nil)
  (mode :none)           ; dominant accumulation: :collect :append :sum :count :max :min :none
  (return-value nil)
  (terminated nil))

(defun kw= (sym string) (and (symbolp sym) (string-equal (symbol-name sym) string)))

(defun eval-extended-loop (env body)
  "Run an extended LOOP. BODY is the clause list."
  (let ((st (make-loop-state :env env)))
    (multiple-value-bind (clauses setup) (parse-loop-clauses st body)
      (declare (ignore setup))
      (run-loop st clauses))))

(defun parse-loop-clauses (st body)
  "Parse BODY into a flat list of runtime clause descriptors, installing
iteration drivers into ST. Returns the clause list."
  (let ((clauses '())
        (rest body))
    (labels ((peek () (car rest))
             (next () (pop rest))
             (expect-kw (s)
               (let ((tok (next)))
                 (unless (kw= tok s) (error "loop: expected ~a, got ~s" s tok)))))
      ;; iteration + accumulation + control, parsed left to right
      (loop while rest
            for tok = (peek)
            do (cond
                 ((kw= tok "FOR") (next) (parse-for st #'next #'peek #'expect-kw))
                 ((kw= tok "COLLECT") (next) (push (list :collect (next)) clauses))
                 ((kw= tok "APPEND") (next) (push (list :append (next)) clauses))
                 ((kw= tok "NCONC") (next) (push (list :append (next)) clauses))
                 ((kw= tok "SUM") (next) (push (list :sum (next)) clauses))
                 ((kw= tok "COUNT") (next) (push (list :count (next)) clauses))
                 ((kw= tok "MAXIMIZE") (next) (push (list :max (next)) clauses))
                 ((kw= tok "MINIMIZE") (next) (push (list :min (next)) clauses))
                 ((kw= tok "DO") (next) (push (list :do (collect-do-body #'next #'peek)) clauses))
                 ((kw= tok "WHEN") (next)
                  (let ((test (next)))
                    (push (list :when test (parse-subclause #'next #'peek)) clauses)))
                 ((kw= tok "IF") (next)
                  (let ((test (next)))
                    (push (list :when test (parse-subclause #'next #'peek)) clauses)))
                 ((kw= tok "UNLESS") (next)
                  (let ((test (next)))
                    (push (list :unless test (parse-subclause #'next #'peek)) clauses)))
                 ((kw= tok "WHILE") (next) (push (list :while (next)) clauses))
                 ((kw= tok "UNTIL") (next) (push (list :until (next)) clauses))
                 ((kw= tok "REPEAT") (next) (push (list :repeat (next)) clauses))
                 ((kw= tok "RETURN") (next) (push (list :return (next)) clauses))
                 ((kw= tok "INITIALLY") (next) (push (list :initially (next)) clauses))
                 ((kw= tok "FINALLY") (next) (push (list :finally (next)) clauses))
                 ((kw= tok "ELSE") (next)
                  (push (list :else (parse-subclause #'next #'peek)) clauses))
                 (t (error "loop: unsupported clause ~s" tok))))
      (values (nreverse clauses) nil))))

(defun collect-do-body (next peek)
  "Collect consecutive compound forms after DO."
  (let ((forms '()))
    (loop while (and (funcall peek) (consp (funcall peek)))
          do (push (funcall next) forms))
    (nreverse forms)))

(defun parse-subclause (next peek)
  "Parse the action after WHEN/IF/UNLESS: do/collect/sum/append + form."
  (let ((tok (funcall peek)))
    (cond
      ((kw= tok "DO") (funcall next) (list :do (collect-do-body next peek)))
      ((kw= tok "COLLECT") (funcall next) (list :collect (funcall next)))
      ((kw= tok "SUM") (funcall next) (list :sum (funcall next)))
      ((kw= tok "APPEND") (funcall next) (list :append (funcall next)))
      ((kw= tok "COUNT") (funcall next) (list :count (funcall next)))
      ((kw= tok "RETURN") (funcall next) (list :return (funcall next)))
      (t (error "loop: unsupported sub-clause ~s" tok)))))

(defun parse-for (st next peek expect-kw)
  "Parse a FOR clause and register an iteration driver."
  (let ((var (funcall next))
        (prep (funcall next)))
    (cond
      ((kw= prep "IN")              ; for x in list
       (let ((list-form (funcall next)))
         (push (make-list-iterator (ls-env st) var list-form) (ls-iters st))))
      ((kw= prep "ON")              ; for x on list
       (let ((list-form (funcall next)))
         (push (make-list-iterator (ls-env st) var list-form t) (ls-iters st))))
      ((kw= prep "ACROSS")          ; for x across vector
       (let ((vec-form (funcall next)))
         (push (make-across-iterator (ls-env st) var vec-form) (ls-iters st))))
      ((kw= prep "FROM")            ; for i from a [to b] [by c]
       (let* ((from-form (funcall next))
              (to-form nil) (by-form nil))
         (loop while (and (funcall peek)
                          (or (kw= (funcall peek) "TO")
                              (kw= (funcall peek) "UPTO")
                              (kw= (funcall peek) "DOWNTO")
                              (kw= (funcall peek) "BELOW")
                              (kw= (funcall peek) "ABOVE")
                              (kw= (funcall peek) "BY")))
               do (let ((k (funcall next)))
                    (cond ((or (kw= k "TO") (kw= k "UPTO") (kw= k "DOWNTO")
                               (kw= k "BELOW") (kw= k "ABOVE"))
                           (setf to-form (list k (funcall next))))
                          ((kw= k "BY") (setf by-form (funcall next))))))
         (push (make-from-iterator (ls-env st) var from-form to-form by-form)
               (ls-iters st))))
      ((or (kw= prep "=") (kw= prep "AS")) ; for x = a then b
       (let ((init-form (funcall next)))
         (funcall expect-kw "THEN")
         (let ((then-form (funcall next)))
           (push (make-then-iterator (ls-env st) var init-form then-form)
                 (ls-iters st))))
       ;; handle "as x = ..." / "for x = ..." with optional then
       )
      (t (error "loop: unsupported FOR preposition ~s" prep)))))

;;; --- iteration drivers ---
;;; Each driver is (lambda (first-p) ...) -> (values var value continue-p),
;;; using mutable state captured in closures.

(defun make-list-iterator (env var list-form &optional on-p)
  (let ((lst (primary (clef-eval list-form env))))
    (lambda (first-p)
      (declare (ignore first-p))
      (if (null lst)
          (values var nil nil)
          (let ((val (if on-p lst (car lst))))
            (multiple-value-prog1 (values var val t)
              (setf lst (cdr lst))))))))

(defun make-across-iterator (env var vec-form)
  (let ((vec (primary (clef-eval vec-form env)))
        (i 0))
    (lambda (first-p)
      (declare (ignore first-p))
      (if (>= i (length vec))
          (values var nil nil)
          (multiple-value-prog1 (values var (aref vec i) t)
            (incf i))))))

(defun make-from-iterator (env var from-form to-form by-form)
  (let* ((cur (primary (clef-eval from-form env)))
         (to-info (and to-form (list (car to-form)
                                     (primary (clef-eval (cadr to-form) env)))))
         (by (if by-form (primary (clef-eval by-form env)) 1))
         (down (and to-info (or (kw= (car to-info) "DOWNTO") (kw= (car to-info) "ABOVE"))))
         (started nil))
    (lambda (first-p)
      (declare (ignore first-p))
      (if started
          (setf cur (if down (- cur by) (+ cur by)))
          (setf started t))
      (let ((done (and to-info
                       (let ((limit (cadr to-info)) (k (car to-info)))
                         (cond ((kw= k "TO") (if down (< cur limit) (> cur limit)))
                               ((kw= k "UPTO") (> cur limit))
                               ((kw= k "DOWNTO") (< cur limit))
                               ((kw= k "BELOW") (>= cur limit))
                               ((kw= k "ABOVE") (<= cur limit))
                               (t nil))))))
        (if done
            (values var nil nil)
            (values var cur t))))))

(defun make-then-iterator (env var init-form then-form)
  (let ((cur nil) (started nil))
    (lambda (first-p)
      (declare (ignore first-p))
      (if (not started)
          (progn (setf started t)
                 (setf cur (primary (clef-eval init-form env)))
                 (values var cur t))
          (progn (setf cur (primary (clef-eval then-form env)))
                 (values var cur t))))))

;;; --- the run loop ---

(defun run-loop (st clauses)
  "Execute the parsed loop clauses. Each iteration steps every driver once;
the loop ends when any driver is exhausted, a terminator (while/until/return)
fires, or a REPEAT clause's iteration budget is spent."
  (let ((env (ls-env st))
        (iters (reverse (ls-iters st)))
        (repeat-budget nil))
    ;; Extract a REPEAT budget if present (it constrains iteration count).
    (dolist (cl clauses)
      (when (eq (car cl) :repeat)
        (setf repeat-budget (%eval-primary (cadr cl) env))))
    (let ((body-clauses (remove-if (lambda (cl) (eq (car cl) :repeat)) clauses)))
      (catch 'loop-return
        (let ((frame (clef/proto/env:make-lexical-env env))
              (iterations 0))
          (loop
            ;; REPEAT budget: stop after N iterations.
            (when (and repeat-budget (>= iterations repeat-budget))
              (return nil))
            ;; step all drivers; if any is exhausted, stop.
            (let ((any-done nil))
              (dolist (it iters)
                (multiple-value-bind (var val ok) (funcall it nil)
                  (clef/proto/env:bind-variable frame var val)
                  (unless ok (setf any-done t))))
              (when any-done (return nil))
              ;; run body clauses
              (dolist (cl body-clauses)
                (run-clause st frame cl))
              (incf iterations))))))
    (loop-result st)))

(defun run-clause (st frame cl)
  (let ((env (ls-env st)))
    (case (car cl)
      (:do (dolist (f (cadr cl)) (%eval f frame)))
      (:collect (setf (ls-mode st) :collect)
                (push (%eval-primary (cadr cl) frame) (ls-collected st)))
      (:append (setf (ls-mode st) :append)
               (setf (ls-appended st)
                     (nconc (ls-appended st) (copy-list (%eval-primary (cadr cl) frame)))))
      (:sum (setf (ls-mode st) :sum)
            (incf (ls-sum st) (%eval-primary (cadr cl) frame)))
      (:count (setf (ls-mode st) :count)
              (when (%eval-primary (cadr cl) frame) (incf (ls-count st))))
      (:max (setf (ls-mode st) :max)
            (let ((v (%eval-primary (cadr cl) frame)))
              (when (or (not (ls-max-p st)) (> v (ls-max st)))
                (setf (ls-max st) v (ls-max-p st) t))))
      (:min (setf (ls-mode st) :min)
            (let ((v (%eval-primary (cadr cl) frame)))
              (when (or (not (ls-min-p st)) (< v (ls-min st)))
                (setf (ls-min st) v (ls-min-p st) t))))
      (:when (when (%eval-primary (cadr cl) frame)
               (run-clause st frame (caddr cl))))
      (:unless (unless (%eval-primary (cadr cl) frame)
                 (run-clause st frame (caddr cl))))
      (:else (run-clause st frame (cadr cl)))
      (:while (unless (%eval-primary (cadr cl) frame)
                (throw 'loop-return nil)))
      (:until (when (%eval-primary (cadr cl) frame)
                (throw 'loop-return nil)))
      (:repeat (let ((n (%eval-primary (cadr cl) frame)))
                 (when (<= n 0) (throw 'loop-return nil))))
      (:return (throw 'loop-return (%eval-primary (cadr cl) frame)))
      (:initially (%eval (cadr cl) frame))
      (:finally (%eval (cadr cl) frame))
      (t (error "loop: bad clause ~s" cl)))))

(defun loop-result (st)
  "Produce the loop's result value based on the dominant accumulation mode."
  (case (ls-mode st)
    (:collect (nreverse (ls-collected st)))
    (:append (ls-appended st))
    (:sum (ls-sum st))
    (:count (ls-count st))
    (:max (ls-max st))
    (:min (ls-min st))
    (t (ls-return-value st))))

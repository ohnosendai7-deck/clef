;;;; test-solver.lisp — µKanren and BDD tests.
;;;;
;;;; CC0 1.0 Universal — public domain. See LICENSE.

(in-package #:clef-test)

(deftest kanren-unify-ground
  (is (nth-value 1 (clef/solver::unify 1 1 clef/solver::+empty-subst+)))
  (is (not (nth-value 1 (clef/solver::unify 1 2 clef/solver::+empty-subst+)))))

(deftest kanren-unify-var
  (let ((x (clef/solver::fresh-var)))
    (multiple-value-bind (s ok) (clef/solver::unify x 42 clef/solver::+empty-subst+)
      (is ok)
      (is (= (clef/solver::walk x s) 42)))))

(deftest kanren-run-conj
  (let ((result (clef/solver::rune (q) (clef/solver::== q 7))))
    (is (equal result '(7)))))

(deftest kanren-conso
  (let ((result (clef/solver::rune (q)
                  (clef/solver::fresh (a d)
                    (clef/solver::conj
                      (clef/solver::conso a d q)
                      (clef/solver::== a 1)
                      (clef/solver::== d '(2 3)))))))
    (is (equal result '((1 2 3))))))

(deftest kanren-disj
  (let ((result (clef/solver::rune (q)
                  (clef/solver::disj
                    (clef/solver::== q 1)
                    (clef/solver::== q 2)))))
    (is (= (length result) 2))
    (is (member 1 result))
    (is (member 2 result))))

(deftest kanren-occurs-check
  ;; x cannot unify with (x) — would be circular.
  (let ((x (clef/solver::fresh-var)))
    (is (not (nth-value 1 (clef/solver::unify x (list x) clef/solver::+empty-subst+))))))

;;;; BDD set-constraint store

(deftest bdd-atom-not-empty
  (is (not (clef/solver::bdd-empty-p (clef/solver::bdd-atom 0)))))

(deftest bdd-and-or-not
  (let ((a (clef/solver::bdd-atom 0))
        (b (clef/solver::bdd-atom 1)))
    ;; a ∧ ¬a = ⊥
    (is (clef/solver::bdd-empty-p
         (clef/solver::bdd-and a (clef/solver::bdd-not a))))
    ;; a ∨ ¬a = ⊤
    (is (= (clef/solver::bdd-or a (clef/solver::bdd-not a))
           clef/solver::*bdd-true*))
    ;; a ∧ b is satisfiable but a ∧ ¬b is not empty
    (is (not (clef/solver::bdd-empty-p (clef/solver::bdd-and a b))))
    (is (not (clef/solver::bdd-empty-p (clef/solver::bdd-and a (clef/solver::bdd-not b)))))))

(deftest subtype-boolean-fragment
  ;; Build types: A = atom0, B = atom1.
  ;; A ∪ B is a supertype of A; A is not a subtype of B; A ∩ B <= A.
  (let ((a (clef/solver::st-atom 0))
        (b (clef/solver::st-atom 1)))
    (is (clef/solver::subtypep-bdd a (clef/solver::st-or a b)))   ; A <= A∪B
    (is (not (clef/solver::subtypep-bdd a b)))                    ; A ≰ B
    (is (clef/solver::subtypep-bdd (clef/solver::st-and a b) a))  ; A∩B <= A
    (is (clef/solver::subtypep-bdd a clef/solver::*st-top*))      ; A <= ⊤
    (is (clef/solver::subtypep-bdd clef/solver::*st-bottom* a)))) ; ⊥ <= A

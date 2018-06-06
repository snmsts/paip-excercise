(uiop/package:define-package :paip-excercise/src/chapter04 (:nicknames)
                             (:use :paip :cl) (:shadow) (:export) (:intern))
(in-package :paip-excercise/src/chapter04)
;;don't edit above
(do-examples 4)

4.1

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))



(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (format *debug-io* "~:[~;~&~v@T~?~]"
          (member id paip::*dbg-ids*)
          (* 2 indent)
          format-string args))

4.2

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  ;;If the input is nil, there is only one permutation:
  ;;nil itself
  (if (null bag)
      '(())
      ;;Otherwise, take an element, e, out of the bag.
      ;;Generate all permutations of the remaining elements.
      ;;And add e to the front of each of these.
      ;;Do this for all possible e to generate all permutations,
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (p) (cons e p))
                          (permutations
                           (remove e bag :count 1 :test #'eq))))
              bag)))
4.3

;(1)
(defparameter *cake-ops*
  (list
   (paip::op 'buy-cake
       :preconds '(have-money)
       :add-list '(have-cake)
       :del-list '(have-money))
   (paip::op 'eat-ice-cream
       :preconds '(have-ice-cream)
       :add-list '(eat-desert)
       :del-list '(have-ice-cream))
   (paip::op 'eat-cake
       :preconds '(have-cake)
       :add-list '(have-ice-cream eat-desert)
       :del-list '(have-cake))))
;;(2)
(paip::gps '(have-money) '(eat-desert) *cake-ops*)

;;(3)

;; change apply-op

(defun gps (state goals &optional (paip::*ops* paip::*ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (paip::find-all-if #'paip::action-p
                     (achieve-all (cons '(paip::start) state) goals nil)))

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, trying several orderings."
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
        (paip::orderings goals)))

(defun achieve-each (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state
                              (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds,
  or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((paip::member-equal goal state) state)
        ((paip::member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (paip::appropriate-ops goal state)))))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (paip::op-action op))
  (let ((state2 (achieve-all state (paip::op-preconds op)
                             (cons goal goal-stack))))
    (unless (null state2)
      ;; Return an updated state
      (dbg-indent :gps (length goal-stack) "Action: ~a ~S ~S" (paip::op-action op) state2  goal-stack)
      (if (paip::member-equal goal state2)
          state2 ;;skip apply if goal is already achieved in recursion.
          (append (remove-if #'(lambda (x)
                                 (paip::member-equal x (paip::op-del-list op)))
                             state2)
                  (paip::op-add-list op))))))

4.4
;;(untrace achieve-all)

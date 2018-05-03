(uiop/package:define-package :paip-excercise/src/chapter03 (:nicknames)
                             (:use :paip :cl) (:shadow)
                             (:import-from :paip-excercise/src/export)
                             (:export) (:intern))
(in-package :paip-excercise/src/chapter03)
;;;don't edit above
(do-examples 3)

3.1
((lambda (x)
   ((lambda (y)
      (+ x y))
    (* x x)))
 6)

3.2
(list* 'a 'b)

#+nil
(defun list* (a &rest r)
  (if r
      (cons a (apply #'list* r))
      r))

3.3

(defun exercise-3.3 (x)
  "Write a function that will print an expression in dotted pair notation.
Use the built-in function princ to print each component of the expression."
  (cond ((consp x)
         (princ "(")
         (exercise-3.3 (first x))
         (princ " . ")
         (exercise-3.3 (rest x))
         (princ ")"))
        (t (princ x)))
  x)

;;(exercise-3.3 '(1 (3 7) . 5))

3.4
(defun exercise-3.4 (x)
  "Write a function that, like the regular print function, will print an
expression in dotted pair notation when necessary but will use normal list notation
when possible."
  (cond ((consp x)
         (princ "(")
         (exercise-3.4 (first x))
         (princ " ")
         (loop for i on (rest x)
            do (exercise-3.4 (first i))
              (when (rest i)
                (princ " ")
                (unless (consp (rest i))
                  (princ ". ")
                  (exercise-3.4 (rest i)))))
         (princ ")"))
        (t (princ x))))

;;(exercise-3.4 '(1 (3 7 8) . 5))

3.5
(defun exercise-3.5 ()
  "(Exercise in altering structure.) Write a program that will play the
role of the guesser in the game Twenty Questions. The user of the program will have
in mind any type of thing. The program will ask questions of the user, which must
be answered yes or no, or \"it\" when the program has guessed it. If the program runs
out of guesses, it gives up and asks the user what \"it\" was. At first the program will
not play well, but each time it plays, it will remember the user's replies and use them
for subsequent guesses."
  )

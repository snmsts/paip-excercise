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
;;skip
(defun exercise-3.5 ()
  "(Exercise in altering structure.) Write a program that will play the
role of the guesser in the game Twenty Questions. The user of the program will have
in mind any type of thing. The program will ask questions of the user, which must
be answered yes or no, or \"it\" when the program has guessed it. If the program runs
out of guesses, it gives up and asks the user what \"it\" was. At first the program will
not play well, but each time it plays, it will remember the user's replies and use them
for subsequent guesses."
  )

3.6
"Given the following initialization for the lexical variable a and the "
(setf a 'global-a) 
(defvar *b* 'global-b) 

(defun fn () *b*) 

(let ((a 'local-a) 
      (*b* 'local-b)) 
  (list a *b* (fn) (symbol-value 'a) (symbol-value'*b*)))
;;-> '(LOCAL-A LOCAL-B LOCAL-B GLOBAL-A LOCAL-B)

3.7
"Why do you think the leftmost of two keys is the one that counts, 
rather than the rightmost?"

;; see answer.

3.8
"Some versions of Kyoto Common Lisp (KCL) have a bug wherein 
they use the rightmost value when more than one keyword/value pair is specified 
for the same keyword. Change the definition of find-a1l so that it works in KCL. 
"
(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
according to the keywords. Doesn't alter sequence."
  (setf test (or (getf keyword-args :test) test))
  (setf test-not (or (getf keyword-args :test-not) test-not))
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

3.9
"Write a version of length using the function reduce. "
(defun length- (list)
  (reduce (lambda (x y)
            (declare (ignore y))
            (1+ x))
          list
          :initial-value 0))

3.10
"Use a reference manual or describe to figure out what the functions lcm and nreconc"

;;lcm := least common multiple
;;nreconc := (nconc (nreverse x) y)

3.11

"There is a built-in Common Lisp function that, given a key, a 
value, and an association List, returns a new association list that is extended to 
include the key/value pair. What is the name of this function?"

;;see:
;;http://www.lispworks.com/documentation/HyperSpec/Body/14_aba.htm
;; googled "assoc acons clhs"

3.12
"Write a single expression using format that will take a list of 
words and print them as a sentence, with the first word capitalized and a period after 
the last word. You will have to consult a reference to learn new format directives."

;;see pcl loop for blackbelts. http://www.gigamonkeys.com/book/loop-for-black-belts.html

;;(format nil "~@(~{~A~^ ~}~)."'(red blue green white black))
;;=> "Red blue green white black."

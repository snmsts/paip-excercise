(uiop/package:define-package :paip-excercise/src/chapter02 (:nicknames)
                             (:use :paip :cl) (:shadow)
                             (:import-from :paip-excercise/src/export)
                             (:export) (:intern))
(in-package :paip-excercise/src/chapter02)
;;don't edit above
(eval-when (:compile-toplevel :load-toplevel :execute)
  (paip:do-examples 2))

2.1
(defun generate1 (phrase)
  "Generate a random sentence or phrase"
  (let (partial)
    (cond ((listp phrase)
           (mappend 'generate1 phrase))
          ((setf partial (rewrites phrase))
           (generate1 (random-elt partial)))
          (t (list phrase)))))

(defun random-elt (x) (first x))
(defun generate2 (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend 'generate2 phrase))
        ((generate2 (random-elt (or (rewrites phrase) '(nil)))))
        (t (list phrase))))

2.2

(defun generate3 (phrase)
  "Generate a random sentence or phrase"
  (cond ((list p phrase)
         (mappend 'generate3 phrase))
        ((non-terminal-p phrase)
         (generate3 (random-elt (rewrites phrase))))
        (t (lis t phrase))))

(setf (symbol-function 'non-terminal-p)
      #'rewrites)

2.3
(defun lisp ()
  (let* ((*grammar*
          `((lisp -> form%)
            (symbol* -> nil (symbol* symbol%))
            (symbol% -> a b c d e f g)
            (form* -> nil (form* form%))
            (form% -> macro% function% sform%)
            (val% -> form% symbol%)
            (function% -> cons% car% cdr%)
            (macro% -> cond% when% unless%)
            (sform% -> if% progn% setq%)
            (setq% -> ([setq symbol% form% ]))
            (list% -> ([list symbol* ]))
            (cons% -> ([cons val% val% ]))
            (car% ->  ([car val% ]))
            (cdr% ->  ([cdr val% ]))
            (cond% -> ([cond cond%%* ]))
            (cond%% -> ([ val% form* ]))
            (cond%%* -> (cond%%* cond%%))
            (when% -> ([when val% form% ]))
            (unless% -> ([unless val% form% ]))
            (if% -> ([if val% form% form% ]) ([if val% form% ]))
            (progn% -> ([progn form* ]))
            (setq% -> ([setq symbol% val% ]))))
         (* (generate 'lisp))
         (* (format nil "~S" *))
         (* (substitute #\Space #\( *))
         (* (substitute  #\Space #\) *))
         (* (substitute #\( #\[ *))
         (* (substitute #\) #\] *)))
    (read-from-string *)))

;;(lisp)

2.4
#+nil
(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (fn x y)) xlist))
           ylist))

#+nil
(defun combine-all (xlist ylist)
  (cross-product #'append xlist ylist))

(uiop/package:define-package :paip-excercise/src/chapter02 (:nicknames)
                             (:use :paip :cl) (:shadow)
                             (:import-from :paip-excercise/src/export)
                             (:export) (:intern))
(in-package :paip-excercise/src/chapter02)
;;don't edit above
(paip:do-examples 2)

2.1

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (let (partial)
    (cond ((listp phrase)
           (mappend #'generate phrase))
          ((setf partial (rewrites phrase))
           (generate (random-elt partial)))
          (t (list phrase)))))

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((generate (random-elt (or (rewrites phrase) '(nil)))))
        (t (list phrase))))
(generate 'phrase)
2.2

(defun generate (phrase)
  "Generate a random sentence or phrase"
  (cond ((list p phrase)
         (mappend #'generate phrase))
        ((non-terminal-p phrase)
         (generate (random-elt (rewrites phrase))))
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
(defun cross-product (fn xlist ylist)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x) (fn x y)) xlist))
           ylist))

(defun combine-all (xlist ylist)
  (cross-product #'append xlist ylist))

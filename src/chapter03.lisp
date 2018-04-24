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


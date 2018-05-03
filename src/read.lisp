(uiop/package:define-package :paip-excercise/src/read (:nicknames) (:use :cl)
                             (:shadow) (:export :excercise) (:intern))
(in-package :paip-excercise/src/read)
;;;don't edit above

(defvar *pages*)
(defvar *Exercise*)

(let ((page 0)
      lines
      pages
      h1
      h2
      id
      Exercise)
  (dolist (line (uiop:read-file-lines
                 (merge-pathnames "PAIP.txt" (asdf:component-pathname (asdf:find-system :paip)))))
    (multiple-value-bind (match b)
        (cl-ppcre:scan-to-strings "^## (.*)[ ]*" line)
      (if match
          (setf h2 nil
                h1 (aref b 0))
          (multiple-value-bind (match b)
              (cl-ppcre:scan-to-strings "^### (.*)[ ]*" line)
            (if match
                (setf h2 (aref b 0))
                (multiple-value-bind (match b)
                    (cl-ppcre:scan-to-strings "<a id='(.*)'></a>" line)
                  (if match
                      (setf id (aref b 0))
                      (multiple-value-bind (match)
                          (cl-ppcre:scan-to-strings "" line)
                        (if match
                            (setf pages (cons (list :page (incf page) :id id
                                                    :h1 h1 :h2 h2
                                                    :text (format nil "~{~A~^~%~}"
                                                                  (nreverse lines)))
                                              pages)
                                  lines nil)
                            (progn
                              (multiple-value-bind (match b)
                                  (cl-ppcre:scan-to-strings "&#9635; Exercise ([0-9\.]*) \\[(.)\\]"
                                                            line)
                                (when match
                                  (push (list :number (aref b 0)
                                              :difficulty (aref b 1)
                                              :id id)
                                        Exercise)))
                              (push line lines)))))))))))
  (setf *pages* (nreverse pages))
  (setf *exercise* (nreverse Exercise)))

(defun find-page (x id)
  (find x *pages*
        :test (lambda (x y) (equal x (getf y id)))))

(defun excercise (&optional number)
  (if number
      (let ((found (find number *exercise* :key (lambda (x) (getf x :number)) :test #'equal)))
        (when found
          (find-page (getf found :id) :id)))
      (mapcar (lambda (x) (list :number (getf x :number)
                            :difficulty (getf x :difficulty)))
              *exercise*)))
;;(excercise "3.2")

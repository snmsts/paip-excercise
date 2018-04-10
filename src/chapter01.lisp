1.1

(defun first-name (name)
  "Select the first name from a name represented as a list."
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defvar *titles* nil)

(defvar *suffix* '(jr md))

(defun reverse- (list)
  (if (null list)
      nil
      (append (reverse- (rest list)) (list (first list)))))

(defun last-name (name)
  "Select the last name from a name represented as a list."
  (let ((*titles* *suffix*))
    (first-name (reverse- name))))

;;(last-name '(Rex Morgan MD))
;;(last-name '(Morton Downey Jr))

1.2
(defun power (x n)
  (if (evenp n)
      (power (* x x)
             (/ n 2))
      (if (= n 0)
          1
          (* x (power x (1- n))))))

1.3
(defun count-atoms (list)
  (let ((num 0))
    (mapcar #'(lambda (x)
                (if (null x)
                    (incf num)
                    (if (listp x)
                        (incf num (count-atoms x))
                        (incf num))))
            list)
    num))

(defun count-atoms (x &optional (null 0))
  (if (null x)
      null
      (if (consp x)
          (+ (count-atoms (first x))
             (count-atoms (rest x)))
          1)))
;;(mapcar (lambda (x) (print x)) '(1 2 . 3))
;;(count-atoms '(1 2 . 3))
;;(consp nil)   () = nil = atom

;;(listp nil)

#+nil(defun listp (x)
       (or (consp x)
           (null x)))

#+nil(defun null (x)
       (equal x nil))


1.5
(defun count-anywhere (elem tree)
  (cond ((consp tree)
         (+ (count-anywhere elem (first tree))
            (count-anywhere elem (rest tree))))
        ((eql elem tree) 1)
        (t 0)))

;;(equal '(a (b c)) '(a (b c)))

;;(count-anywhere '(a (b c)) '(d a (b c)))
#+nil
(defun dot-product (a b)
  (apply #'+ (mapcar #'* a b)))
;;(dot-product '(10 20) '(3 4))

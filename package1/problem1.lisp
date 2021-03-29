;;; Exercise 1.A
(format T "The 3th element of the list is ~d ~%" (nth 3 (list 1 2 3 4 5)))
;;; Exercise 1.B
(format T "A leap-year has ~d seconds ~%" (* 366 24 60 60))
;;; Exercise 1.C
(defvar y 20)
(defvar x 10)
(format T "~d ~%" (and (/= x 0) (<= x y)))
;;; Exercise 1.D
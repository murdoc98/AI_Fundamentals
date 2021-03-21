;;; Exercise 3.A
;;; Output: (TWO) -- Not commonly used nowadays
(format T "~d ~%" (cdar '((one two) three four)))
;;; Exercise 3.B
;;; Output: ((EVA LISA) KARL SVEN EVA LISA KARL SVEN)
(format T "~d ~%" (append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven)))
;;; Exercise 3.C
;;; Output: (EVA GITAN LISA GITAN KARIN)
(format T "~d ~%" (subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin)))
;;; Exercise 3.D
;;; Output: (EVA LISA ANNA)
(format T "~d ~%" (remove 'sven '(eva sven lisa sven anna)))
;;; Exercise 3.E
;;; Output: (KARL ADAM NILSSON)
(format T "~d ~%" (butlast '(karl adam nilsson gregg alisson vilma) 3))
;;; Exercise 3.F
;;; Ouput: C
(format T "~d ~%" (nth 2 '(a b c d e)))
;;; Exercise 3.G
;;; Ouput: (C D E)
(format T "~d ~%" (nthcdr 2 '(a b c d e)))
;;; Exercise 3.H
;;; Output: (C B)
(format T "~d ~%" (intersection '(a b c) '(x b z c)))
;;; Exercise 3.I
;;; Output: (4)
(format T "~d ~%" (cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8))))))
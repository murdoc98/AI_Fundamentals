;;; Problem 1
;;; Exercise 1.A
(format T "The 3th element of the list is ~d ~%" (nth 3 (list 1 2 3 4 5)))
;;; Exercise 1.B
(format T "A leap-year has ~d seconds ~%" (* 366 24 60 60))
;;; Exercise 1.C
(defvar y 20)
(defvar x 10)
(format T "~d ~%" (and (/= x 0) (<= x y)))
;;; Exercise 1.D

;;; Problem 2
;;; Exercise 2.A
(format T "~d ~%" (+ (* 2 4) (- 6 8)))
;;; Exercise 2.B
(format T "~d ~%" (/ (+ 5 (+ -3 4) (+ 6 (/ 2 5)))))
;;; Exercise 2.C
(format T "~d ~%" (sqrt (/ (- 1.4502 (* -1 (+ 4 (/ 3 8)))) (expt -1 (expt (- 3 5) (/ 1 3))))))
;;; Exercise 2.D
(format T "~d ~%" (expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7)))

;;; Problem 3
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

;;; Problem 4
(defvar res NIL)
(defvar aux NIL)
(defvar aux2 NIL)
(defun recombina
    (l1)
    (push (cdr (nth 2 l1)) aux)
    (push (cdr (nth 1 l1)) aux)
    (push (cdr (nth 0 l1)) aux)
    (setq aux2 (nth 0 (nth 1 l1)))
    (push (cons aux aux2) res)
    (setq aux NIL)
    (push (cdr (nth 2 l1)) aux)
    (push (cdr (nth 1 l1)) aux)
    (setq aux2 (nth 0 (nth 2 l1)))
    (push (cons aux aux2) res)
    (setq aux NIL)
    (push (cdr (nth 1 l1)) aux)
    (push (cdr (nth 0 l1)) aux)
    (setq aux2 (nth 0 (nth 0 l1)))
    (push (cons aux aux2) res)
    
)
(format T "~d ~%" (recombina '((A . x) (B . y) (C . z)) ))

;;; Problem 5
(defun realnocero (n) (/= n 0))
;;; Retorna NIL
(format T "~d ~%" (realnocero 1))
;;; Retorna T
(format T "~d ~%" (realnocero 0))

;;; Problem 6
(defvar res NIL)
(defun Analyze 
    (var)
    (setq res NIL)
    (push (atom var) res)
    (push (equal 5 var) res)
    (push (listp var) res)
    (push (consp var) res)
    (push (null var) res)
    (format T "~d ~%" res)
)
(Analyze 5)
(Analyze NIL)
(Analyze '(1 2 3))

;;; Problem 7
(defun intercala (l1 l2 &optional (res NIL))
    (if (and (not (null l1)) (not (null l2)))
        (progn
            (setq res (append res (list (first l1))))
            (setq res (append res (list (first l2))))
            (intercala (rest l1) (rest l2) res)
        )
        (progn
            (append res l1)
            (append res l2)
        )
    )
)
(defvar res NIL)
(setq res (intercala '(1 2 3 4) '(5 6 7 8 9 10)))
(format T "~d ~%" res)

;;; Problem 8
(defun sameType (l1 l2)
    (if (and 
            (not (null l1))
            (not (null l2))
        )
        (if (equal (first l1) (first l2))
            (sameType (rest l1) (rest l2))
            NIL
        )
        T
    )
)
(format T "~d ~%" (sameType '(1 2 3 4) '(1 2 3 4)))
(format T "~d ~%" (sameType '(1 2 3 4) '(5 6 7 8)))

;;; Problem 9
(defun apalindrome (l)
    (format T "~d ~%" (reverse l))
    (format T "~d ~%" l)
    (equal l (reverse l))
)
(format T "~d ~%" (apalindrome '(A n i t a l a v a l a t i \n A)))
(format T "~d ~%" (apalindrome '(A n i t a l a v a l a t i n a)))

;;; Problem 10
(defun leap-year (year)
    (format T "Año ~d ~%" year)
    (or (equal 0 (mod year 400)) (and (equal 0 (mod year 4)) (/= 0 (mod year 100))))
)
(format T "Es bisiesto: ~d ~%" (leap-year 1700))
(format T "Es bisiesto: ~d ~%" (leap-year 2000))
(format T "Es bisiesto: ~d ~%" (leap-year 2008))
(format T "Es bisiesto: ~d ~%" (leap-year 2010))
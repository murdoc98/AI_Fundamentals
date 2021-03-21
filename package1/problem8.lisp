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
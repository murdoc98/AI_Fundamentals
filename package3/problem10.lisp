(defun numericSum
    (l1 &optional (total 0))
    (if 
        (equal l1 NIL)
        total
        (if
            (numberp (first l1))
            (numericSum (rest l1) (+ total (first l1)))
            (numericSum (rest l1) total)
        )
    )
)
(format T "Suma total: ~d ~%" (numericSum '(5 2 5 4 5)))
(format T "Suma total: ~d ~%" (numericSum '(5 2 5 '(A B) 4 5 NIL)))
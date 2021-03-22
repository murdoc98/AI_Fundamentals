(defun firstOdd
    (l1 &optional (index 0))
    (if
        (equal l1 NIL)
        NIL
        (if
            (/= 0 (mod (first l1) 2))
            (list (first l1) index)
            (firstOdd (rest l1) (+ index 1))
        )
    )
)
(format T "Concurrencia: ~d ~%" (firstOdd '(6 2 3 4 5)))
(format T "Concurrencia: ~d ~%" (firstOdd '(6 2 4 8)))
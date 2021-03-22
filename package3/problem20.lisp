(defun deleteOnList
    (l1 num1 &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (progn
            (if
                (and (numberp (first l1)) (< num1 (first l1)))
                (push (first l1) res)
            )
            (deleteOnList (rest l1) num1 res)
        )
    )
)
(format T "Ocurrencias: ~d ~%" (deleteOnList '(1 2 3 4 5) 3))
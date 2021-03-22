(defun change
    (l1 elem1 elem2 &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (if
            (equal (first l1) elem1)
            (change (rest l1) elem1 elem2 (push elem2 res))
            (change (rest l1) elem1 elem2 (push (first l1) res))
        )
    )

)
(format T "Multiplos: ~d ~%" (change '(1 2 3 4 5 6 7 5 9 5) 5 6))
(defun filterMultiples
    (l1 num &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (progn
            (if
                (equal 0 (mod (first l1) num))
                (push (first l1) res)
            )
            (filterMultiples (rest l1) num res)
        )
    )
)
(format T "Multiplos: ~d ~%" (filterMultiples '(1 2 3 4 5 6) 2))
(format T "Multiplos: ~d ~%" (filterMultiples '(1 2 3 4 5 6) 1))
(format T "Multiplos: ~d ~%" (filterMultiples '(1 2 3 4 5 6) 3))
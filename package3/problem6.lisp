(defun counter
    (l1 &optional (atoms 0) (lists 0))
    (if
        (equal l1 NIL)
        (cons atoms lists)
        (progn
            (cond
                ((equal (first l1) NIL) (incf atoms 1) (incf lists 1))
                ((atom (first l1)) (incf atoms 1))
                ((listp (first l1)) (incf lists 1))
            )
            (counter (rest l1) atoms lists)
        )
    )
)
(format T "Ocurrencias: ~d ~%" (counter '(5 2 5 4 5)))
(format T "Ocurrencias: ~d ~%" (counter '(5 2 NIL 4 NIL '(A B C))))
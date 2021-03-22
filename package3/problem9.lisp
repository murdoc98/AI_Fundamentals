(defun getTypeInList
    (l1 &optional (newList NIL))
    (if
        (equal l1 NIL)
        (reverse newList)
        (progn
            (cond
                ((equal (first l1) NIL) (push 'N newList))
                ((atom (first l1)) (push 'A newList))
                ((listp (first l1)) (push 'L newList))
            )
            (getTypeInList (rest l1) newList)
        )
    )
)
(format T "Ocurrencias: ~d ~%" (getTypeInList '(5 2 5 4 5)))
(format T "Ocurrencias: ~d ~%" (getTypeInList '(5 2 NIL 4 NIL '(A B C))))
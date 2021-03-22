(defun lastBiggerThanZero
    (l1 &optional (value NIL) (total 0))
    (if 
        (equal l1 NIL)
        (list value total)
        (if
            (equal value NIL)
            (if 
                (>= (first (last l1)) 0)
                (lastBiggerThanZero (butlast l1) (first (last l1)) 1)
                (lastBiggerThanZero (butlast l1))
            )
            (if
                (equal (first (last l1)) value)
                (lastBiggerThanZero (butlast l1) value (incf total 1))
                (lastBiggerThanZero (butlast l1) value total)
            )
        )   
    )
)
(format T "Ocurrencias: ~d ~%" (lastBiggerThanZero '(5 2 5 4 5)))
(defun aplana
    (orList &optional (res 0))
    (loop for i from 0 to (- (length orList) 1) by 1
        do(if
            (consp (nth i orList))
            (incf res 1)
            (if 
                (listp (nth i orList))
                (incf res (aplana (nth i orList)))
            )
        )
    )
    res
)
(format T "Multiplos: ~d ~%" (aplana '(1 2 3 4 5 (6 . 8) (6 (5 (7 . 9))))))
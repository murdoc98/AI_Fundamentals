(defvar aux)
(defun aplana
    (orList &optional (res NIL))
    (loop for i from 0 to (- (length orList) 1) by 1
        do(if
            (atom (nth i orList))
            (push (nth i orList) res)
            (progn
                (setq aux (aplana (nth i orList)))
                (loop for j from 0 to (- (length aux) 1) by 1 do
                    (push (nth j aux) res)
                )
            )
        )
    )
    (reverse res)
)

(format T "Multiplos: ~d ~%" (aplana '(1 (a 2 (8 e NIL 10)) 3 4 5 6)))
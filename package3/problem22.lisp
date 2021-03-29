(defun qsort
    (l1)
    (if
        (equal (length l1) 1)
        (progn
            l1
        )
        (let ((i 0) (res NIL) (left NIL) (right NIL) (center NIL))
            (loop for j from 1 to (- (length l1) 1) by 1 do
                (if
                    (>= (nth j l1) (nth 0 l1))
                    (progn
                        (incf i 1)
                        (rotatef (nth i l1) (nth j l1))
                    )
                )
            )
            (rotatef (nth i l1) (nth 0 l1))
            (setq left (subseq l1 (+ i 1) (length l1)))
            (setq center (nth i l1))
            (setq right (subseq l1 0 i))
            ;;;(format T "Valor derecho: ~d ~%" left)
            ;;;(format T "Valor central: ~d ~%" center)
            ;;;(format T "Valor izquierdo: ~d ~%" right)
            (if
                (not (equal left NIL))
                (push (qsort left) res)
            )
            (push center res)
            (if
                (not (equal right NIL))
                (push (qsort right) res)
            )
            (aplana res)
        )
    )
)
(defun aplana
    (orList &optional (res NIL))
    (let (aux)
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
)
(format T "Arreglo ordenado: ~d ~%" (qsort '(4 3 2 1)))
(format T "Arreglo ordenado: ~d ~%" (qsort '(1 2 3 4)))
(format T "Arreglo ordenado: ~d ~%" (qsort '(10 7 5 3 12 22)))

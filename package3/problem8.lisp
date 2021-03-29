(defun diagonal
    (l1 &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (progn
            (push (first (first l1)) res)
            (loop for i from 0 to (- (length l1) 1) by 1 do 
                (setf (nth i l1) (rest (nth i l1)) )
            )
            (diagonal (rest l1) res)
        )
    )
)

(format T "Diagonal: ~d ~%" (diagonal '((1 2 3) (4 5 6) (7 8 9))))
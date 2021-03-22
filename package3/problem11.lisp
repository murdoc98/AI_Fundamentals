(defvar aux)
(defvar aux2 '(a e i o u))
(defun removeVowels
    (orList &optional (res NIL))
    (loop for i from 0 to (- (length orList) 1) by 1
        do(if
            (atom (nth i orList))
            (if
                (not (member (nth i  orList) aux2))
                (push (nth i orList) res)
            )
            (push (removeVowels (nth i orList)) res)
        )
    )
    (reverse res)
)
(format T "Multiplos: ~d ~%" (removeVowels '(1 (a 2 (8 e NIL 10)) 3 4 5 6)))
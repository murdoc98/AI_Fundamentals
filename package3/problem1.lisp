(defun elemInPosRec 
    (elem list pos)
    (if
        (equal 0 pos)
        (equal elem (first list))
        (elemInPosRec elem (rest list) (- pos 1))
    )
)
(format T "Elemento es igual: ~d ~%" (elemInPosRec 3 '(1 2 'A 4 5) 7))
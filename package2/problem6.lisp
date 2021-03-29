(defvar nums 0)
(defvar lists 0)
(defun counter
    (l1)
    (loop for i from 0 to (- (length l1) 1) by 1 do
        (if
            (numberp (nth i l1))
            (progn
                (setq nums (incf nums 1))
                (format T "Encontro un numero :c ~d~%"  (nth i l1))
            )
        )
        (if
            (listp (nth i l1))
            (progn
                (setq lists (incf lists 1))
                (format T "Encontro un lista :c ~d~%"  (nth i l1))
            )
        )
    )
    (cons nums lists)
)
(format T "conteo de elementos: ~d ~%" (counter '(1 2 3 (1 2 (1 2 3 4)) NIL 5)))
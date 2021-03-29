;; [[ Paquete#2 ]]

(defvar cellCons 0)

(defun Celdas (lista)

    (loop for i from 0 to (- (length lista) 1) by 1 do
        
        (if (consp (nth i lista))
            
            (incf cellCons 1)

        )
    )

    cellCons  
)

(format T "~d" (Celdas '(a b c (a . b) (b . c) d)))
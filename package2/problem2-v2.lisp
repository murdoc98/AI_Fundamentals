
;; [[ Paquete#2 ]]
(defparameter pos NIL)
(defparameter j 0)

(defun Inicio-en (lista elem)
    (loop for i from 0 to (- (length lista) 1) by 1 do
        (if
            (equal (nth i lista) elem)
            (progn
                (format T "Si jalo ewe ~d  ~d ~%" elem (nth i lista))
                (setq pos i)
                (return)
            )
            (format T "No jalo :c ~d  ~d ~%" elem (nth i lista))
        )
    )
    (format T "Posicion de la primera incidencia ~d ~%" pos)
    (nthcdr pos lista)
)
(format T "~d ~%" (Inicio-en '(1 2 2 3 4 5 6) 2))

;; [[ Paquete#2 ]]


;; Definicion de Variables
(defvar i 0)
(defvar verificador 0)
(defvar columnas NIL)
(defvar diagonal NIL)
(defvar diagonalDef NIL)



;; Definicion de funcion
;; Argumentos: Lista
(defun Diagonal (lista)

    ;; Verificamos si la matriz es cuadrada m x m
    (loop while (nth i lista) do

        (progn
            ;;
            (setq columnas (nth i lista))

            (if (eql (length lista) (length columnas))

                (setq verificador (incf verificador 1))
            )
            
            (setq i (incf i 1))
        )
    )

    (setq i 0)


    ;; Si la matriz es cuadrada procedemos
    (if (eql (length lista) verificador)

        (progn

            ;; Relaizar la  diagonal
            (loop while (nth i lista) do 

                (progn
            
                    (setq columnas(nth i lista))
                    (push (nth i columnas) diagonal)
                    (setq i (incf i 1))
                )
            )

            ;(setq i 0)

            ;; Invertir la diagonal
        )

    )

    ;; Si la matriz no es cuadrada
    (if (eql i 0)
    
        (print "La matriz no es cuadrada m x m")

    )

    (reverse diagonal)
)

(format T "~d" (Diagonal '((a b c) (f d d) (h 1 2 q))))


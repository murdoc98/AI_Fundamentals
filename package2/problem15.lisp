;; [[Paquete#2]]
(defvar verificadorM1 0)
(defvar verificadorM2 0)
(defvar noMatriz1 0)
(defvar noMatriz2 0)
(defvar numColM1 0)
(defvar numFilM2 0)
(defvar fila 0)
(defvar columna 0)
(defvar multMatriz NIL)
(defvar suma 0)
(defvar multMatrizDef Nil)


(defun Mult (matriz1 matriz2)

    ;; Verificadores
    (setq verificadorM1 (length (nth 0 matriz1)))
    (setq verificadorM2 (length (nth 0 matriz2)))

    ;;Verificar que todas las filas tengan el mismo numero de columnas
    (loop for i from 0 to (- (length matriz1) 1) by 1 do

        (progn


            ;; Si al menos una fila es distinta de la primera, quiere decir que no es una matriz

            (if (/= verificadorM1 (length (nth i matriz1))) 

                ; No es una matriz
                (setq noMatriz1 1)
            )

        )
    
    )
    
    ;;Verificar que todas las filas tengan el mismo numero de columnas
    (loop for i from 0 to (- (length matriz2) 1) by 1 do

        (progn

            ;; Verificador sera igual al tamaño de cada fila de la amtriz
            

            ;; Si al menos una fila es distinta de la primera, quiere decir que no es una matriz
            
            ; (format T "Verificador ~d ~%" verificadorM2)
            ; (format T "Tamaño ~d ~%" (length (nth i matriz2)))

            (if (/= verificadorM2 (length (nth i matriz2))) 

                ; No es una matriz
                (setq noMatriz2 1)
            )

        )
    
    )

    ;; Verificar que el numero de columnas de la primera matriz sea igual al numero de filas de la segunda matriz
    ;; NOTA: Una fila es igual a una sublista en la lista


    ;  Fila (1 2 3) -- (nth i matriz)
    ;  Fila (4 5 6)
    ;  Fila (7 8 9)
    ;
    ; Columna (nth j (nth i matriz))
    
    (if (and (equal noMatriz1 0) (equal noMatriz2 0))

        (progn

            ;; (nth columna (nth fila matriz))
            (loop for i from 0 to (- (length matriz1) 1) do ;; Fila

                (loop for j from 0 to  (- (length (nth i matriz2)) 1) do ;; Columna

                    (loop for k from 0 to (length (nth 0 matriz2)) do
                    
                        (progn
                            
                            (setq fila (nth k (nth i matriz1)))
                            (setq columna (nth j (nth k matriz2)))
                            (setq suma (+ suma (* fila columna)))
                        )
                    )
                    
                    (push suma multMatriz)
                    (setq suma 0)
                    
                )
                
                (push (reverse multMatriz) multMatrizDef)
                (setq multMatriz NIL)
            )
        )
    )

    (if (or (/= noMatriz1 0) (/= noMatriz2 0))

        (write "Una de las matrices impide hacer la operacion")

    )

    (reverse multMatrizDef)

)



(defvar matriz1 '(
                    (5 3 -4 -2)
                    (8 -1 0 -3)
                ) 
)

(defvar matriz2 '(
                    (1 4 0)
                    (-5 3 7)
                    (0 -9 5)
                    (5 1 4) 
                ) 
)


(format T "~d" (Mult matriz1 matriz2))
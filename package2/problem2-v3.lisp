;; [[ Paquete#2 ]]

;; Definicion de variables
(defparameter pos 0)

;; Definicion de funcion
;; Parametros: Una lista // Un elemento cualquiera
(defun Inicio-en (lista elem)

    ;; Ciclo For desde i = 0 hasta la longitud de la lista en pasos de 1
    (loop for i from 0 to (length lista) by 1 do

        ;; Condicion if si el elemento n-esimo de la lista es igual al elemento elem
        (if(eql (nth i lista) elem)
            
            ;; Si es asi, asignamos el numero de posision donde se encuentra la primera ocurrencia 
            ;; A una variable pos y terminamos la instruccion if
            (progn 
                (setq pos i)
                (return)
            )
        )
    )

    ;; La funcion [nthcdr] nos permite eliminar los n primeros elementos de una lista
    (nthcdr pos lista)
    
)

(format T "~d ~%" (Inicio-en '(1 2 2 3 4 5 6) 2))


;;[[ Paquete#2 ]]

;; Definicion de variables
(defparameter pos 0)

;; Definicion de funciones
;; Parametros: Una lista // un elemento cualquiera
(defun Terminar-en (lista elem)

    ;; Ciclo For desde i = 0 hasta la longitud de la lista en pasos de 1
    (loop for i from 0 to (length lista) by 1 do
        
        ;; Condicion if si el elemento n-esimo de la lista es igual al elemento elem
        (if (eql (nth i lista) elem)

            ;; Si es asi, asignamos el numero de posision donde se encuentra la primera ocurrencia 
            ;; A una variable pos
            (setq pos i)

        )
    )

    ;; Hacemos una diferencia del tamaño de la lista - 1 - pos y la reasignamos a pos
    (setq pos (- (-(length lista) 1) pos))
    
    ;; butlast permite eleminar los n ultimos elementos de la lista
    (butlast lista pos)
   
)

(format T "~d ~%" (Terminar-en '(1 2 2 2 2) 2))
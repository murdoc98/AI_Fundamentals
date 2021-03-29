;;[[ Paquete#2 ]]

;; [[ Paquete#2 ]]

;; Definicion de variables globales
(defparameter elem -1)
(defparameter concu 0)



;; Definicion de funcion
;; Parametros: Recibe una lista
(defun Primer-impar (lista)

    ;;  Ciclo For desde i = 0 hasta la longitud de la lista en pasos de 1
    (loop for i from 0 to (- (length lista) 1) by 1 do
    
        ;; Condicion IF
        ;; Si el modulo 2 del elemento de n-simo de la lista es distinto de 0 
        ;; Indica que es un numero impar
        (if (>= (nth i lista) 0)

            (setq elem (nth i lista))
        )

    )

    (loop for i from 0 to (- (length lista) 1) by 1 do
    
        (if (and (eql elem (nth i lista)) (>= elem 0))

            (setq concu (+ concu 1))

        )
    
    )
    ;; Creamos una lista con el elemento impar y la posicion en la que corresponde 
    (list elem concu) 
)

(format T "(Elemento de la lista | #Ocurrencias ) -> ~d" (Primer-impar '(5 5 5 5 -1)))

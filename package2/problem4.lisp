;; [[ Paquete#2 ]]

;; Definicion de variables globales
(defparameter pos Nil)
(defparameter elem Nil)



;; Definicion de funcion
;; Parametros: Recibe una lista
(defun Primer-impar (lista)

    ;;  Ciclo For desde i = 0 hasta la longitud de la lista en pasos de 1
    (loop for i from 0 to (- (length lista) 1) by 1 do
    
        ;; Condicion IF
        ;; Si el modulo 2 del elemento de n-simo de la lista es distinto de 0 
        ;; Indica que es un numero impar
        (if (/= (mod (nth i lista) 2) 0)
            
            ;; progn permite realizar varias instrucciones en un solo parentesis de un IF
            (progn
                
                ;; Variable elem sera igual al primer elemento impar de la lista
                (setq elem (nth i lista))
                ;; Varaible pos sera igual al indice en el cual se localiza el elemento impar
                (setq  pos i)
                ;; Terminamos la instruccion if
                (return)
            )
        )
    )
    ;; Creamos una lista con el elemento impar y la posicion en la que corresponde 
    (list elem pos)
)

(format T "(Elemento de la lista | Indice ) -> ~d ~%" (Primer-impar '(2 2)))
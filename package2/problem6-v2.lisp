;;[[ Paquete#2 ]]

;;Definimos variables
(defvar contNums 0)
(defvar contSubList 0)

;; Definicion de funcion
;; Parametros: Lista

(defun Conteo (lista)

    (loop for i from 0 to (- (length lista) 1) by 1 do
        

        ;; numberp permite encontrar elementos numericos de una lista
        (if (numberp (nth i lista))
    
            (setq contNums (incf contNums 1))
            
        )

        ;; listp permite encontrar sublistas dentro de listas
        (if (listp (nth i lista))
            
            (setq contSubList (incf contSubList 1))

        )
    )
    ;; Asignamos valores a una celda de construccion
    (cons contNums contSubList)
)
(format T "#Elememntos numericos en la lista original . #De Sublistas: ~d ~%" (Conteo '(1 2 3 (1 2 (1 2 3 4)) NIL 5)))
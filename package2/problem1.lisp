;; [[ Paquete#2 ]]


;; Variables Globales
(defvar a NIL)

;; Definicion de la funcion
(defun ElemInPos (elem lista pos)
   
    ;; Ciclo para recorrer la lista desde i igual a 0 hasta [pos] en pasos de 1 en 1
    (loop for i from 0 to pos by 1
      ;; Asignamos [ la variable a el elemento [i] de la lista
      do(setq a (nth i lista))

    )

    ;; Evaluamos si es igual el elemento buscado [elem] se encontro en la posicion [pos] de la lista
    (eq elem a)
)

;; Invocacion de funcion 
;; Argumentos: [Elemnto buscado][Lista][Posicion en donde se busca el elemento]
(format T "El elemento [elem] SI esta en la posicion [pos] de la lista : [T]   ~%")
(format T "El elemento [elem] NO esta en la posicion [pos] de la lista : [NIL] ~%")
(format T "Estado: ~d ~%" (ElemInPos 2 '(1 2 3 4) 1))





;; [[ Paquete#2 ]]

;; Definimos variables
(defvar listaNueva NIL)
(defvar i 0)
(defvar suma 0)

;; Definicion de funcio
;; Parametros: lista
(defun flatten (lista)

   (do* 
      ; Asignacion de variables 
      (
         (resultado (list lista))
         (node resultado)
      )
      
      (
         (null node) 
         (delete nil resultado)
      )

      ;; cond permite realizar evaluaciones e instrucciones
      (cond 
      
         (  
            ;consp permite verificar si un dato es de tipo cons
            (consp (car node))
      
            
            (when 
                  
               (cdar node) 
               (push (cdar node) 
                  (cdr node)
               )
            )
      
            (setf (car node) (caar node))
         )

         (t (setf node (cdr node)))
      )
   )
)

;; Definicion de Funcion
;; Parametros: lista
(defun Suma-numerica (lista)

    (setq listaNueva (flatten lista))

    (loop while (nth i listaNueva) do

        (progn

            (if (numberp (nth i listaNueva))

                (setq suma (incf suma (nth i listaNueva)))
            
            )
        )

        (setq i (incf i 1))
    )

    (return-from Suma-numerica suma)

)


(format T "~d" (Suma-numerica '(1 2 (3 4 (3 4 a)))))
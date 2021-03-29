;; [[ Paquete#2 ]]

;; [[ Paquete#2 ]]

(defvar i 0)
(defvar newLista NIL)
(defvar listaDef NIL)

(defun Aplana (lista)

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

(defun Filtra-multiplos (lista numero)

    (setq lista (Aplana lista))

    (loop for i from 0 to (- (length lista) 1) by 1 do


        (if (> (mod (nth i lista) numero) 0)

            (progn

                (push (nth i lista) newLista)
            )
            
        )
    )

    (loop while (nth i newlista) do 

        (progn

            (push (nth i newLista) listaDef)
            (setq i (incf i 1))

        )

    )
    (return-from Filtra-multiplos listaDef)
)

(format T "~d" (Filtra-multiplos '(2 4 5 6 6 (10 11 12) 6 6 6) 2))
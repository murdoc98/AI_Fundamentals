;; [[ Paquete#2 ]]

(defun Aplana (obj)

   (do* 
      ; Asignacion de variables 
      (
         (resultado (list obj))
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

(format T "~d" (Aplana '(a b c d (e f g (h 1)))))
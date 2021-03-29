;; [[ Paquete#2 ]]

(defvar listaVocales '(a e i o u A E I O U))
(defvar i 0)
(defvar j 0)


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

(defun remove-nth (list n)
  (remove-if (constantly t) list :start n :end (1+ n))
)

(defun Filtra-vocales (lista)

    (setq lista(Aplana lista))
    (print lista)

    (loop while (nth i lista) do

        (progn

            (setq j 0)

            (loop while (nth j listaVocales) do
            
                (progn

                    ;(print (nth j listaVocales))

                    (if (equal (nth i lista) (nth j listaVocales) )
                    
                        (progn
                        
                            (setq lista (remove-nth lista i))
                        )
                    
                    )

                )

                (setq j (incf j 1))

            )

        )

        (setq i (incf i 1))

    )

    (return-from Filtra-vocales lista)
)

(format T "~d" (Filtra-vocales '((a (a e i) b c) (d (a e i) e f) (g h i))))
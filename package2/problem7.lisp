(defun flatten (obj)
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
            (format T "~d~%" node)
            
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
(format T "~d" (flatten '(a (1 2 3 4) c d (e f g (h 1)))))


; (do 
   
;    ;;Variables e Instrucciones
   
;    (
;       (x 0 (+ 2 x))
;       (y 20 ( - y 2))
;    )

;    ;; Condicion de parada
;    (  
;       ;; Parar hasta que x & y sean iguales 
;       (= x y)
;       ;; Hacer restar x - y
;       (- x y)
;    )
;    (format t "~% x = ~d  y = ~d" x y)
; )

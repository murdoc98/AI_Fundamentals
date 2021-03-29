;;; Problem 1
(defvar a NIL)
(defun ElemInPos (elem lista pos)
    (loop for i from 0 to pos by 1
      do(setq a (nth i lista))
    )
    (eq elem a)
)
(format T "Estado: ~d ~%" (ElemInPos 2 '(1 2 3 4) 1))


;;; Problem 2
(defun initOn
    (elem list)
    (loop while (not (equal list NIL)) do(if
        (equal (first list) elem)
        (return)
        (setq list (rest list))
    ))
    list
)
(format T "Nueva lista: ~d ~%" (initOn 3 '(1 2 3 3 4 5)))

;;; Problem 3
(defparameter pos 0)
(defun Terminar-en (lista elem)
    (loop for i from 0 to (length lista) by 1 do
        (if (eql (nth i lista) elem)
            (setq pos i)
        )
    )
    (setq pos (- (-(length lista) 1) pos))
    (butlast lista pos)
)
(format T "~d ~%" (Terminar-en '(1 2 2 2 2) 2))

;;; Problem 4
(defparameter pos Nil)
(defparameter elem Nil)
(defun Primer-impar (lista)
    (loop for i from 0 to (- (length lista) 1) by 1 do
        (if (/= (mod (nth i lista) 2) 0)
            (progn
                (setq elem (nth i lista))
                (setq  pos i)
                (return)
            )
        )
    )
    (list elem pos)
)
(format T "(Elemento de la lista | Indice ) -> ~d ~%" (Primer-impar '(2 2)))

;;; Problem 5
(defparameter elem -1)
(defparameter concu 0)
(defun Primer-impar (lista)
    (loop for i from 0 to (- (length lista) 1) by 1 do
        (if (>= (nth i lista) 0)
            (setq elem (nth i lista))
        )
    )
    (loop for i from 0 to (- (length lista) 1) by 1 do
        (if (and (eql elem (nth i lista)) (>= elem 0))
            (setq concu (+ concu 1))
        )
    )
    (list elem concu) 
)
(format T "(Elemento de la lista | #Ocurrencias ) -> ~d~%" (Primer-impar '(5 5 5 5 -1)))

;;; Problem 6
(defvar nums 0)
(defvar lists 0)
(defun counter
    (l1)
    (loop for i from 0 to (- (length l1) 1) by 1 do
        (if
            (numberp (nth i l1))
            (progn
                (setq nums (incf nums 1))
                (format T "Encontro un numero :c ~d~%"  (nth i l1))
            )
        )
        (if
            (listp (nth i l1))
            (progn
                (setq lists (incf lists 1))
                (format T "Encontro un lista :c ~d~%"  (nth i l1))
            )
        )
    )
    (cons nums lists)
)
(format T "conteo de elementos: ~d ~%" (counter '(1 2 3 (1 2 (1 2 3 4)) NIL 5)))

;;; Problem 7
(defun flatten (obj)
    (do*
      (
         (resultado (list obj))
         (node resultado)
      )
      (
         (null node) 
         (delete nil resultado)
      )
      (cond 
         (
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
(format T "~d~%" (flatten '(a (1 2 3 4) c d (e f g (h 1)))))

;;; Problem 8
(defvar i 0)
(defvar verificador 0)
(defvar columnas NIL)
(defvar diagonal NIL)
(defvar diagonalDef NIL)
(defun Diagonal (lista)
    (loop while (nth i lista) do
        (progn
            (setq columnas (nth i lista))
            (if (eql (length lista) (length columnas))

                (setq verificador (incf verificador 1))
            )
            (setq i (incf i 1))
        )
    )
    (setq i 0)
    (if (eql (length lista) verificador)

        (progn
            (loop while (nth i lista) do 

                (progn
            
                    (setq columnas(nth i lista))
                    (push (nth i columnas) diagonal)
                    (setq i (incf i 1))
                )
            )
        )
    )
    (if (eql i 0)
        (format T "La matriz no es cuadrada m x m~%")
    )
    (reverse diagonal)
)
(format T "~d~%" (Diagonal '((a b c) (f d d) (h 1 2 q))))

;;; Problem 9
(defvar res NIL)

(defun getTypeInList
    (l1)
    (setq res NIL)
    (loop for i from 0 to (- (length l1) 1) by 1 do
        (cond 
            ; Si es una lista vacia
            ((equal (nth i l1) NIL)(push 'N res))
            ; Si es un atomo
            ((atom (nth i l1) )(push 'A res))
            ; Si es una lista
            ((listp (nth i l1))(push 'L res))
        )
    )
    (reverse res)
)
(format T "Ocurrencias: ~d ~%" (getTypeInList '(5 2 5 4 5)))
(format T "Ocurrencias: ~d ~%" (getTypeInList '(5 2 NIL 4 NIL '(A B C))))

;;; Problem 10
(defvar listaNueva NIL)
(defvar i 0)
(defvar suma 0)

(defun flatten (lista)
   (do* 
      (
         (resultado (list lista))
         (node resultado)
      )
      
      (
         (null node) 
         (delete nil resultado)
      )
      (cond 
         (  
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
(format T "~d~%" (Suma-numerica '(1 2 (3 4 (3 4 a)))))

;;; Problem 11
(defvar listaVocales '(a e i o u A E I O U))
(defvar i 0)
(defvar j 0)
(defun Aplana (lista)
   (do* 
      (
         (resultado (list lista))
         (node resultado)
      )
      (
         (null node) 
         (delete nil resultado)
      )
      (cond 
         (  
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
(format T "~d~%" (Filtra-vocales '((a (a e i) b c) (d (a e i) e f) (g h i))))

;;; Problem 12
(defvar i 0)
(defvar newLista NIL)
(defvar listaDef NIL)
(defun Aplana (lista)
   (do* 
      (
         (resultado (list lista))
         (node resultado)
      )
      (
         (null node) 
         (delete nil resultado)
      )
      (cond 
         (  
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
(format T "~d~%" (Filtra-multiplos '(2 4 5 6 6 (10 11 12) 6 6 6) 2))

;;; Problem 13
(defvar cellCons 0)
(defun Celdas (lista)
    (loop for i from 0 to (- (length lista) 1) by 1 do
        (if (consp (nth i lista)) 
            (incf cellCons 1)
        )
    )
    cellCons  
)
(format T "~d~%" (Celdas '(a b c (a . b) (b . c) d)))

;;; Problem 14

;;; Problem 15
(defvar verificadorM1 0)
(defvar verificadorM2 0)
(defvar noMatriz1 0)
(defvar noMatriz2 0)
(defvar numColM1 0)
(defvar numFilM2 0)
(defvar fila 0)
(defvar columna 0)
(defvar multMatriz NIL)
(defvar suma 0)
(defvar multMatrizDef Nil)
(defun Mult (matriz1 matriz2)
    (setq verificadorM1 (length (nth 0 matriz1)))
    (setq verificadorM2 (length (nth 0 matriz2)))
    (loop for i from 0 to (- (length matriz1) 1) by 1 do
        (progn
            (if (/= verificadorM1 (length (nth i matriz1))) 
                (setq noMatriz1 1)
            )
        )
    )
    (loop for i from 0 to (- (length matriz2) 1) by 1 do
        (progn
            (if (/= verificadorM2 (length (nth i matriz2))) 
                (setq noMatriz2 1)
            )
        )
    )
    (if (and (equal noMatriz1 0) (equal noMatriz2 0))
        (progn
            (loop for i from 0 to (- (length matriz1) 1) do
                (loop for j from 0 to  (- (length (nth i matriz2)) 1) do
                    (loop for k from 0 to (length (nth 0 matriz2)) do
                        (progn
                            (setq fila (nth k (nth i matriz1)))
                            (setq columna (nth j (nth k matriz2)))
                            (setq suma (+ suma (* fila columna)))
                        )
                    )
                    (push suma multMatriz)
                    (setq suma 0) 
                )
                (push (reverse multMatriz) multMatrizDef)
                (setq multMatriz NIL)
            )
        )
    )
    (if (or (/= noMatriz1 0) (/= noMatriz2 0))
        (write "Una de las matrices impide hacer la operacion")
    )
    (reverse multMatrizDef)
)
(defvar matriz1 '(
                    (5 3 -4 -2)
                    (8 -1 0 -3)
                ) 
)
(defvar matriz2 '(
                    (1 4 0)
                    (-5 3 7)
                    (0 -9 5)
                    (5 1 4) 
                ) 
)
(format T "~d~%" (Mult matriz1 matriz2))
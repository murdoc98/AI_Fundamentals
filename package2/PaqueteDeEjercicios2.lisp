;;; Ejercicio 1
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
;;; Ejercicio 2
;; [[ Paquete#2 ]]

;; Definicion de variables
(defparameter pos 0)

;; Definicion de funcion
;; Parametros: Una lista // Un elemento cualquiera
(defun Inicio-en (lista elem)

    ;; Ciclo For desde i = 0 hasta la longitud de la lista en pasos de 1
    (loop for i from 0 to (length lista) by 1 do

        ;; Condicion if si el elemento n-esimo de la lista es igual al elemento elem
        (if(eql (nth i lista) elem)
            
            ;; Si es asi, asignamos el numero de posision donde se encuentra la primera ocurrencia 
            ;; A una variable pos y terminamos la instruccion if
            (progn 
                (setq pos i)
                (return)
            )
        )
    )

    ;; La funcion [nthcdr] nos permite eliminar los n primeros elementos de una lista
    (nthcdr pos lista)
    
)

(format T "~d ~%" (Inicio-en '(1 2 2 3 4 5 6) 2))
;;; Ejercicio 3
;;[[ Paquete#2 ]]

;; Definicion de variables
(defparameter pos 0)

;; Definicion de funciones
;; Parametros: Una lista // un elemento cualquiera
(defun Terminar-en (lista elem)

    ;; Ciclo For desde i = 0 hasta la longitud de la lista en pasos de 1
    (loop for i from 0 to (length lista) by 1 do
        
        ;; Condicion if si el elemento n-esimo de la lista es igual al elemento elem
        (if (eql (nth i lista) elem)

            ;; Si es asi, asignamos el numero de posision donde se encuentra la primera ocurrencia 
            ;; A una variable pos
            (setq pos i)

        )
    )

    ;; Hacemos una diferencia del tamaño de la lista - 1 - pos y la reasignamos a pos
    (setq pos (- (-(length lista) 1) pos))
    
    ;; butlast permite eleminar los n ultimos elementos de la lista
    (butlast lista pos)
   
)

(format T "~d ~%" (Terminar-en '(1 2 2 2 2) 2))
;;; Ejercicio 4
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
;;; Ejercicio 5
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
(format T "(Elemento de la lista | #Ocurrencias ) -> ~d~%" (Primer-impar '(5 5 5 5 -1)))
;;; Ejercicio 6
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
;;; Ejercicio 7
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

(format T "~d~%" (Aplana '(a b c d (e f g (h 1)))))
;;; Ejercicio 8
;; [[ Paquete#2 ]]


;; Definicion de Variables
(defvar i 0)
(defvar verificador 0)
(defvar columnas NIL)
(defvar diagonal NIL)
(defvar diagonalDef NIL)



;; Definicion de funcion
;; Argumentos: Lista
(defun Diagonal (lista)

    ;; Verificamos si la matriz es cuadrada m x m
    (loop while (nth i lista) do

        (progn
            ;;
            (setq columnas (nth i lista))

            (if (eql (length lista) (length columnas))

                (setq verificador (incf verificador 1))
            )
            
            (setq i (incf i 1))
        )
    )

    (setq i 0)


    ;; Si la matriz es cuadrada procedemos
    (if (eql (length lista) verificador)

        (progn

            ;; Relaizar la  diagonal
            (loop while (nth i lista) do 

                (progn
            
                    (setq columnas(nth i lista))
                    (push (nth i columnas) diagonal)
                    (setq i (incf i 1))
                )
            )

            ;(setq i 0)

            ;; Invertir la diagonal
        )

    )

    ;; Si la matriz no es cuadrada
    (if (eql i 0)
    
        (print "La matriz no es cuadrada m x m")

    )

    (reverse diagonal)
)

(format T "~d~%" (Diagonal '((a b c) (f d d) (h 1 2 q))))
;;; Ejercicio 9
;; [[ Paquete#2 ]]

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
;;; Ejercicio 10
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
(format T "~d~%" (Suma-numerica '(1 2 (3 4 (3 4 a)))))
;;; Ejercicio 11
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

(format T "~d~%" (Filtra-vocales '((a (a e i) b c) (d (a e i) e f) (g h i))))
;;; Ejercicio 12
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

(format T "~d~%" (Filtra-multiplos '(2 4 5 6 6 (10 11 12) 6 6 6) 2))
;;; Ejercicio 13
;; [[ Paquete#2 ]]

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
;;; Ejercicio 14

;;; Ejercicio 15
;; [[Paquete#2]]
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

    ;; Verificadores
    (setq verificadorM1 (length (nth 0 matriz1)))
    (setq verificadorM2 (length (nth 0 matriz2)))

    ;;Verificar que todas las filas tengan el mismo numero de columnas
    (loop for i from 0 to (- (length matriz1) 1) by 1 do

        (progn


            ;; Si al menos una fila es distinta de la primera, quiere decir que no es una matriz

            (if (/= verificadorM1 (length (nth i matriz1))) 

                ; No es una matriz
                (setq noMatriz1 1)
            )

        )
    
    )
    
    ;;Verificar que todas las filas tengan el mismo numero de columnas
    (loop for i from 0 to (- (length matriz2) 1) by 1 do

        (progn

            ;; Verificador sera igual al tamaño de cada fila de la amtriz
            

            ;; Si al menos una fila es distinta de la primera, quiere decir que no es una matriz
            
            ; (format T "Verificador ~d ~%" verificadorM2)
            ; (format T "Tamaño ~d ~%" (length (nth i matriz2)))

            (if (/= verificadorM2 (length (nth i matriz2))) 

                ; No es una matriz
                (setq noMatriz2 1)
            )

        )
    
    )

    ;; Verificar que el numero de columnas de la primera matriz sea igual al numero de filas de la segunda matriz
    ;; NOTA: Una fila es igual a una sublista en la lista


    ;  Fila (1 2 3) -- (nth i matriz)
    ;  Fila (4 5 6)
    ;  Fila (7 8 9)
    ;
    ; Columna (nth j (nth i matriz))
    
    (if (and (equal noMatriz1 0) (equal noMatriz2 0))

        (progn

            ;; (nth columna (nth fila matriz))
            (loop for i from 0 to (- (length matriz1) 1) do ;; Fila

                (loop for j from 0 to  (- (length (nth i matriz2)) 1) do ;; Columna

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

;;; Ejercicio 1
(defun ElemInPos 
    (elem list pos)
    (if
        ;;; Preguntamos si la possicion es igual a 0
        (equal 0 pos)

        (equal elem (first list))
        ;;; Si no es asi modificamos el elemento
        (ElemInPos elem (rest list) (- pos 1))
    )
)

(format T "Elemento es igual: ~d ~%" (ElemInPos 3 '(1 2 'A 4 5) 7))

;;; Ejercicio 2
(defun Inicio-en
    (elem list)
    (if
        ;;; Si el elemento es igual al primer elemento regresamos la lista
        (equal elem (first list))
        list
        ;;; Si no preguntamos si la lista esta vacia y de no ser el caso hacemos recursion
        (if
            (equal list NIL)
            NIL 
            (Inicio-en elem (rest list))
        )
    )
)
(format T "Nueva lista: ~d ~%" (Inicio-en 3 '(1 2 3 4 5)))
(format T "Nueva lista: ~d ~%" (Inicio-en 6 '(1 2 3 4 5)))

;;; Ejercicio 3
(defun Terminar-en
    (elem list)
    (if
        (equal elem (first (last list)))
        list
        (if
            (equal list NIL)
            NIL
            (Terminar-en elem (butlast list))
        )
    )
)
(format T "Nueva lista: ~d ~%" (Terminar-en 3 '(1 2 3 4 5)))

;;; Ejercicio 4
(defun Primer-impar
    (l1 &optional (index 0))
    (if
        (equal l1 NIL)
        NIL
        (if
            (/= 0 (mod (first l1) 2))
            (list (first l1) index)
            (Primer-impar (rest l1) (+ index 1))
        )
    )
)
(format T "Concurrencia: ~d ~%" (Primer-impar '(6 2 3 4 5)))
(format T "Concurrencia: ~d ~%" (Primer-impar '(6 2 4 8)))

;;; Ejercicio 5
(defun ultimoMayor
    (l1 &optional (value NIL) (total 0))
    (if 
        (equal l1 NIL)
        (list value total)
        (if
            (equal value NIL)
            (if 
                (>= (first (last l1)) 0)
                (ultimoMayor (butlast l1) (first (last l1)) 1)
                (ultimoMayor (butlast l1))
            )
            (if
                (equal (first (last l1)) value)
                (ultimoMayor (butlast l1) value (incf total 1))
                (ultimoMayor (butlast l1) value total)
            )
        )   
    )
)
(format T "Ocurrencias: ~d ~%" (ultimoMayor '(5 2 5 4 5)))

;;; Ejercicio 6
(defun Conteo
    (l1 &optional (atoms 0) (lists 0))
    (if
        (equal l1 NIL)
        (cons atoms lists)
        (progn
            (cond
                ((equal (first l1) NIL) (incf atoms 1) (incf lists 1))
                ((atom (first l1)) (incf atoms 1))
                ((listp (first l1)) (incf lists 1))
            )
            (Conteo (rest l1) atoms lists)
        )
    )
)
(format T "Ocurrencias: ~d ~%" (Conteo '(5 2 5 4 5)))
(format T "Ocurrencias: ~d ~%" (Conteo '(5 2 NIL 4 NIL '(A B C))))

;;; Ejercicio 7
(defun aplana
    (orList &optional (res NIL))
    (let (aux)
        (loop for i from 0 to (- (length orList) 1) by 1
            do(if
                (atom (nth i orList))
                (push (nth i orList) res)
                (progn
                    (setq aux (aplana (nth i orList)))
                    (loop for j from 0 to (- (length aux) 1) by 1 do
                        (push (nth j aux) res)
                    )
                )
            )
        )
        (reverse res)
    )
)

(format T "Multiplos: ~d ~%" (aplana '(1 (a 2 (8 e NIL 10)) 3 4 5 6)))

;;; Ejercicio 8
(defun diagonal
    (l1 &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (progn
            (push (first (first l1)) res)
            (loop for i from 0 to (- (length l1) 1) by 1 do 
                (setf (nth i l1) (rest (nth i l1)) )
            )
            (diagonal (rest l1) res)
        )
    )
)

(format T "Diagonal: ~d ~%" (diagonal '((1 2 3) (4 5 6) (7 8 9))))

;;; Ejercicio 9
(defun getTypeInList
    (l1 &optional (newList NIL))
    (if
        (equal l1 NIL)
        (reverse newList)
        (progn
            (cond
                ((equal (first l1) NIL) (push 'N newList))
                ((atom (first l1)) (push 'A newList))
                ((listp (first l1)) (push 'L newList))
            )
            (getTypeInList (rest l1) newList)
        )
    )
)
(format T "Ocurrencias: ~d ~%" (getTypeInList '(5 2 5 4 5)))
(format T "Ocurrencias: ~d ~%" (getTypeInList '(5 2 NIL 4 NIL '(A B C))))

;;; Ejercicio 10
(defun numericSum
    (l1 &optional (total 0))
    (if 
        (equal l1 NIL)
        total
        (if
            (numberp (first l1))
            (numericSum (rest l1) (+ total (first l1)))
            (numericSum (rest l1) total)
        )
    )
)
(format T "Suma total: ~d ~%" (numericSum '(5 2 5 4 5)))
(format T "Suma total: ~d ~%" (numericSum '(5 2 5 '(A B) 4 5 NIL)))

;;; Ejercicio 11
(defvar aux)
(defvar aux2 '(a e i o u))
(defun removeVowels
    (orList &optional (res NIL))
    (loop for i from 0 to (- (length orList) 1) by 1
        do(if
            (atom (nth i orList))
            (if
                (not (member (nth i  orList) aux2))
                (push (nth i orList) res)
            )
            (push (removeVowels (nth i orList)) res)
        )
    )
    (reverse res)
)
(format T "Multiplos: ~d ~%" (removeVowels '(1 (a 2 (8 e NIL 10)) 3 4 5 6)))

;;; Ejercicio 12
(defun filterMultiples
    (l1 num &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (progn
            (if
                (equal 0 (mod (first l1) num))
                (push (first l1) res)
            )
            (filterMultiples (rest l1) num res)
        )
    )
)
(format T "Multiplos: ~d ~%" (filterMultiples '(1 2 3 4 5 6) 2))
(format T "Multiplos: ~d ~%" (filterMultiples '(1 2 3 4 5 6) 1))
(format T "Multiplos: ~d ~%" (filterMultiples '(1 2 3 4 5 6) 3))

;;; Ejercicio 13
(defun Celdas
    (orList &optional (res 0))
    (loop for i from 0 to (- (length orList) 1) by 1
        do(if
            (consp (nth i orList))
            (incf res 1)
            (if 
                (listp (nth i orList))
                (incf res (Celdas (nth i orList)))
            )
        )
    )
    res
)
(format T "Multiplos: ~d ~%" (Celdas '(1 2 3 4 5 (6 . 8) (6 (5 (7 . 9))))))

;;; Ejercicio 14


;;; Ejercicio 15
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

;;; Ejercicio 16
(defun change
    (l1 elem1 elem2 &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (if
            (equal (first l1) elem1)
            (change (rest l1) elem1 elem2 (push elem2 res))
            (change (rest l1) elem1 elem2 (push (first l1) res))
        )
    )

)
(format T "Multiplos: ~d ~%" (change '(1 2 3 4 5 6 7 5 9 5) 5 6))

;;; Ejercicio 17
(defun fib1 (n)
  "Naive recursive computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (if (< n 2) n
      (+ (fib1 (1- n)) (fib1 (- n 2)))))

(defun fib2 (n)
  "Tail-recursive computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n f1 f2)
                    (if (zerop n) f1
                      (fib-aux (1- n) f2 (+ f1 f2)))))
          (fib-aux n 0 1)))
    
(defun fib3 (n)
  "loop-based iterative computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (loop for f1 = 0 then f2
        and f2 = 1 then (+ f1 f2)
        repeat n finally (return f1)))

(defun fib4 (n)
  "do-based iterative computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (do ((i n (1- i))
       (f1 0 f2)
       (f2 1 (+ f1 f2)))
      ((= i 0) f1)))
    
(defun fib5 (n)
  "CPS computation of the nth element of the Fibonacci sequence"
  (check-type n (integer 0 *))
  (labels ((fib-aux (n k)
                    (if (zerop n)
                        (funcall k 0 1)
                      (fib-aux (1- n) (lambda (x y)
                                        (funcall k y (+ x y)))))))
          (fib-aux n #'(lambda (a b) a))))

(defun fib6 (n)
   (labels ((fib2 (n)
                 (cond ((= n 0)
                        (values 1 0))
                       (t
                        (multiple-value-bind (val prev-val)
                                             (fib2 (- n 1))
                           (values (+ val prev-val)
                                   val))))))
      (nth-value 0 (fib2 n))))

(defun fib7 (n)
  "Successive squaring method from SICP"
  (check-type n (integer 0 *))
  (labels ((fib-aux (a b p q count)
                    (cond ((= count 0) b)
                          ((evenp count)
                           (fib-aux a
                                    b
                                    (+ (* p p) (* q q))
                                    (+ (* q q) (* 2 p q))
                                    (/ count 2)))
                          (t (fib-aux (+ (* b q) (* a q) (* a p))
                                      (+ (* b p) (* a q))
                                      p
                                      q
                                      (- count 1))))))
          (fib-aux 1 0 0 1 n)))

(defun fib8 (n)
  (if (< n 2) n
    (if (oddp n) 
      (let ((k (/ (1+ n) 2)))
        (+ (expt (fib8 k) 2) (expt (fib8 (1- k)) 2)))
      (let* ((k (/ n 2)) (fk (fib8 k)))
        (* (+ (* 2 (fib8 (1- k))) fk) fk)))))

(time (fib8 50))
(time (fib7 50))
(time (fib6 50))
(time (fib5 50))
(time (fib4 50))
(time (fib3 50))
(time (fib2 50))
;;;(time (fib1 50))

;;; Ejercicio 18


;;; Ejercicio 19
(defvar aux)
(defun aplana
    (orList &optional (res NIL))
    (loop for i from 0 to (- (length orList) 1) by 1
        do(if
            (atom (nth i orList))
            (push (nth i orList) res)
            (progn
                (setq aux (aplana (nth i orList)))
                (loop for j from 0 to (- (length aux) 1) by 1 do
                    (push (nth j aux) res)
                )
            )
        )
    )
    (reverse res)
)

(format T "Multiplos: ~d ~%" (aplana '(1 (a 2 (8 e NIL 10)) 3 4 5 6)))

;;; Ejercicio 20
(defun deleteOnList
    (l1 num1 &optional (res NIL))
    (if
        (equal l1 NIL)
        (reverse res)
        (progn
            (if
                (and (numberp (first l1)) (< num1 (first l1)))
                (push (first l1) res)
            )
            (deleteOnList (rest l1) num1 res)
        )
    )
)
(format T "Ocurrencias: ~d ~%" (deleteOnList '(1 2 3 4 5) 3))

;;; Ejercicio 21
(defun pasteNChange
    (l1 l2 num1 num2 &optional (res NIL))
    (if
        (and (equal l1 NIL) (equal l2 NIL))
        (reverse res)
        (if
            (not (equal l1 NIL))
            (progn
                (if
                    (equal (first l1) num1)
                    (push num2 res)
                    (push (first l1) res)
                )
                (pasteNChange (rest l1) l2 num1 num2 res)
            )
            (progn
                (if
                    (equal (first l2) num1)
                    (push num2 res)
                    (push (first l2) res)
                )
                (pasteNChange l1 (rest l2) num1 num2 res)
            )
        )
    )
)
(format T "Ocurrencias: ~d ~%" (pasteNChange '(1 2 3 4 5) '(1 2 3 4 5) 3 4))

;;; ejercicio 22
(defun qsort
    (l1)
    (if
        (equal (length l1) 1)
        (progn
            l1
        )
        (let ((i 0) (res NIL) (left NIL) (right NIL) (center NIL))
            (loop for j from 1 to (- (length l1) 1) by 1 do
                (if
                    (>= (nth j l1) (nth 0 l1))
                    (progn
                        (incf i 1)
                        (rotatef (nth i l1) (nth j l1))
                    )
                )
            )
            (rotatef (nth i l1) (nth 0 l1))
            (setq left (subseq l1 (+ i 1) (length l1)))
            (setq center (nth i l1))
            (setq right (subseq l1 0 i))
            ;;;(format T "Valor derecho: ~d ~%" left)
            ;;;(format T "Valor central: ~d ~%" center)
            ;;;(format T "Valor izquierdo: ~d ~%" right)
            (if
                (not (equal left NIL))
                (push (qsort left) res)
            )
            (push center res)
            (if
                (not (equal right NIL))
                (push (qsort right) res)
            )
            (aplana res)
        )
    )
)
(format T "Arreglo ordenado: ~d ~%" (qsort '(4 3 2 1)))
(format T "Arreglo ordenado: ~d ~%" (qsort '(1 2 3 4)))
(format T "Arreglo ordenado: ~d ~%" (qsort '(10 7 5 3 12 22)))
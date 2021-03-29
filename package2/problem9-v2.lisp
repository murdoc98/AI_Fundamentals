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
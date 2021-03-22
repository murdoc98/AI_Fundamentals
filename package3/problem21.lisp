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
(defun finishOn
    (elem list)
    (if
        (equal elem (first (last list)))
        list
        (if
            (equal list NIL)
            NIL
            (finishOn elem (butlast list))
        )
    )
)
(format T "Nueva lista: ~d ~%" (finishOn 3 '(1 2 3 4 5)))
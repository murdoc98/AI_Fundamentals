(defun initOn
    (elem list)
    (if
        (equal elem (first list))
        list
        (if
            (equal list NIL)
            NIL 
            (initOn elem (rest list))
        )
    )
)
(format T "Nueva lista: ~d ~%" (initOn 3 '(1 2 3 4 5)))
(format T "Nueva lista: ~d ~%" (initOn 6 '(1 2 3 4 5)))
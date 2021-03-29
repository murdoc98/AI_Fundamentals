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
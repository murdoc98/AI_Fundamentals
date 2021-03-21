(defvar res NIL)
(defun Analyze 
    (var)
    (setq res NIL)
    (push (atom var) res)
    (push (equal 5 var) res)
    (push (listp var) res)
    (push (consp var) res)
    (push (null var) res)
    (format T "~d ~%" res)
)

(Analyze 5)
(Analyze NIL)
(Analyze '(1 2 3))
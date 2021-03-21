(defun apalindrome (l)
    (format T "~d ~%" (reverse l))
    (format T "~d ~%" l)
    (equal l (reverse l))
)

(format T "~d ~%" (apalindrome '(A n i t a l a v a l a t i \n A)))
(format T "~d ~%" (apalindrome '(A n i t a l a v a l a t i n a)))
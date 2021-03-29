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
(time (fib1 50))
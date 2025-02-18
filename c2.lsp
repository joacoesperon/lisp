(defun last (L)
    (cond ((null (cdr L)) (car L)) ;estoy en el ultimo elemento cuadno cdr es null
           (t (last (cdr L)))))

(defun len (l)
    (cond ((null l) 0))
          (t (+1 (len (cdr l)))))

(defun contains (x l)
    (cond ((null l) nil)
          ((equal x (car l)) t)
          (t (contains x (cdr l)))))    

(defun exp (m n)
    (cond ((= n 0) 1)
          (t (* m (exp m (- n 1))))))

(defun fib (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (t (+ (fib (- n 1))
                (fib (- n 2))))))


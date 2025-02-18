;retorna el ultimo elemento de la lista
(defun last (L)
    (cond ((null (cdr L)) (car L)) ;estoy en el ultimo elemento cuadno cdr es null
           (t (last (cdr L)))))

;retorna la longitud de la lista
(defun len (l)
    (cond ((null l) 0))
          (t (+1 (len (cdr l)))))

;retorna t si x pertenece a l, f otherwise
(defun contains (x l)
    (cond ((null l) nil)
          ((equal x (car l)) t)
          (t (contains x (cdr l)))))    

;retorna exponential (m^n)
(defun exp (m n)
    (cond ((= n 0) 1)
          (t (* m (exp m (- n 1))))))

;
(defun fib (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (t (+ (fib (- n 1))
                (fib (- n 2))))))

;
(defun divide (m n)
    (cond ((< m n) 0)
          (t (+1 (divide (- m n) n)))))

;
(defun par (n)
    (cond ((= (* 2 (dividir n 2)) n) t)
          (t nil)))

;
(defun impar (1)
    (cond ((null l) nil)
          ((not (par (car l)))
           (cons (car l) (impar (cdr l))))
           (t (impar (cdr l)))))

;
(defun borrar (x l)
    (cond  ((null ) nil)
           ((equal x (car l)) (cdr l))
           (t (cons (car l)
                    (borrar x (cdr l))))))        
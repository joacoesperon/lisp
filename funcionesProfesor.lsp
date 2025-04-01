
;///////////////////////////////////////
;//  funciones basica y condicionales //
;///////////////////////////////////////

;cuadrado de un numero
(defun cuadrado (n) (* n n))

; tercer, que avalua al tercer element d’una llista:
(defun tercer (l)
 (car (cdr (cdr l))))

; ultimo, que avalua al ultimo element d’una llista:
(defun ultimo (l)
 (car (reverse l)))

; hacer par, retorna n+1 si n es impar , n si es par
(defun hacerpar (n)
    (cond   ((oddp n) (+ n 1))
            (t n)))

; absoluto retorna el valor absoluto
(defun absolut (n)
    (cond   ((>= n 0) n)
            (t (- n))))

;///////////////////////////////////////
;//  funciones con recursividad       //
;///////////////////////////////////////


;retorna el ultimo elemento de la lista sin usar reverse
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

;retorna en enesimo valor de la serie de fibonacci
(defun fib (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (t (+ (fib (- n 1))
                (fib (- n 2))))))

; divide m/n
(defun divide (m n)
    (cond ((< m n) 0)
          (t (+1 (divide (- m n) n)))))

; 
(defun par (n)
    (cond ((= (* 2 (dividir n 2)) n) t)
          (t nil)))

; dada una lista de numeros , nos retorna la lista con los impares eliminados
(defun impar (1)
    (cond ((null l) nil)
          ((not (par (car l)))
           (cons (car l) (impar (cdr l))))
           (t (impar (cdr l)))))

; elimina la primera aparicion de un elemento dentro de una lista
(defun borrar (x l)
    (cond  ((null ) nil)
           ((equal x (car l)) (cdr l))
           (t (cons (car l)
                    (borrar x (cdr l))))))        

; elimina todas las apariciones de un elemento dentro de una lista
(defun borrar-todo (x l)
    (cond   ((null l) nil)
            ((equal x (car l))
             (borrar-todo x (cdr l)))
            (t (cons (car l)
                     (borrar-todo x (cdr l))))))

; retorna todos los elementos de una lista menos el ultimo
(defun rdc (l)
    (cond ((null (cdr l)) nil)
          (t (cons (car l) (rdc (cdr l))))))

; añade un elemento al final de la lista;snoc
(defun añadir (x l)
    (cond ((null l) (cons x nil))
          (t (cons (car l) (añadir x (cdr l))))))

; escala multiplica todos los elementos de una lista por un numero
(defun escala (x l)
    (cond ((null l) nil)
            (t (cons (* x (car l))
                     (escala x (cdr l))))))

;   maximo de una lista
(defun maximo (l)
    (cond ((null (cdr l)) (car l))
            ((> (car l ) (maximo (cdr l))) (car l))
            (t (maximo (cdr l)))))

; minimo de una lista
(defun minimo (l)
    (cond ((null (cdr l)) (car l))
          ((< (car l) (minimo (cdr l))) (car l))
          (t (minimo (cdr l)))))

;ordenar ordena una lista de menor a mayor con insercion directa
(defun ordenar (l)
    (cond   ((null l) nil)
            (t (cons (minimo l)
                     (ordenar (borrar (minimo l) l))))))

;invertir retorna una lista al reves
(defun invertir (l)
    (cond   ((null l) nil)
            ((null (cdr l)) l)
            (t (append (invertir (cdr l))
                        (list (car l))))))

;borrar n borra el enesimo elemnto de una lista
(defun borrarn (n l)
    (cond ((= n 1) (cdr l))
            (t (cons (car l)
                     (borrarn (- n 1) (cdr l))))))

;veces cuenta el numero de veces que n aparece en l
(defun veces (x l)
    (cond ((null l) 0)
            ((equal x (car l))
            (+ 1 (veces x (cdr l))))
            (t (veces x (cdr l)))))

;atomos cuenta el numero de atomos que hay en una lista
(defun atomos (l)
    (cond ((null l) 0)
            ((listp (car l))
            (+ (atomos (car l)) (atomos (cdr l))))
            (t (+ 1 (atomos (cdr l))))))

; convierte una lista que tiene listas como elementos en una sola lista
; (aplana '(a (b (c d) e) f g (h i))) → (a b c d e f g h i)
(defun aplana (l)
    (cond ((null l) l)
            ((atom (car l))
            (cons (car l) (aplana (cdr l))))
            (t (append (aplana (car l))
                        (aplana (cdr l))))))

; sin-primeros-n retorna la misma lista sin los primeros n elementos
(defun sin-primeros-n (n l)
    (cond ((= n 0) l)
            (t (sin-primeros-n (- n 1) (cdr l)))))

;primeros-n dada una lista retorna los primeros n elementos
(defun primeros-n (n l)
    (cond ((= n 0) nil)
            (t (cons (car l)
                     (primeros-n (- n 1) (cdr l))))))

; insertar inserta un elemento en la enesima posicioned la lista
(defun insertar(que donde l)
    (cond ((= on 1) (cons que l))
            (t (cons (car l)
                     (insertar que
                                (- on 1)
                                (cdr l))))))

;cambia el enesimo elemento de una lista por un valor dado
(defun cambiar(on l por)
    (cond ((= on 1) (cons por (cdr l)))
            (t (cons (car l)
                     (cambiar (- on 1)
                              (cdr l)
                              por)))))

; inserta a la izquierda de un elemento un elemento
(defun insertar-izquierda (deque que l)
    (cond ((null l) nil)
          ((equal  (car l) deque) (cons que l))
          (t (cons (car l)
                    (insertar-izquierda deque que (cdr l))))))

; inserta a la derecha de un elemento un elemento
(defun insertar-derecha (deque que l)
    (cond ((null l) nil)
          ((equal (car l) deque)
          (cons (car l) (cons que (cdr l))))
          (t (cons (car l) (insertar-derecha deque que (cdr l))))))                

; pag 77
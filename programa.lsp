; Función principal para generar el laberinto
(defun inicio (&optional (n 25) (m 25))
  (let ((laberinto (inicializar-matriz-laberinto n m)))
    ; Generamos la entrada aleatoria
    (let ((resultado (generar-entrada laberinto n m)))
      (let ((laberinto-modificado (car resultado)) ; Primer elemento: matriz
            (casilla-actual (car (cdr resultado)))) ; Segundo elemento: lista (fila columna)
           
           
        ; Extraemos fila y columna de casilla-actual
        (let ((fila-entrada (car casilla-actual))
              (columna-entrada (car (cdr casilla-actual))))
          ; Mostramos la posición de la entrada y algunos valores para verificar
          (format t "Posición de entrada (~d, ~d): ~a~%" 
                  fila-entrada columna-entrada
                  (obtener-elemento laberinto-modificado fila-entrada columna-entrada))        
          (format t "Posición (0, 0): ~a~%" (obtener-elemento laberinto-modificado 0 0))
          (format t "Posición (24, 24): ~a~%" (obtener-elemento laberinto-modificado 24 24))
          ))))) ; Retornamos la matriz modificada

; Función para generar la entrada aleatoria evitando los bordes
(defun generar-entrada (matriz n m)
  (let ((fila-entrada (numero-aleatorio n))
        (columna-entrada (numero-aleatorio m)))
    (let ((laberinto-modificado (establecer-elemento matriz fila-entrada columna-entrada 'entrada)))
      (list laberinto-modificado (list fila-entrada columna-entrada))))) ; Retorna matriz y casilla actual

; Función para generar un número aleatorio entre 1 y frontera-2 (evita bordes)
(defun numero-aleatorio (frontera)
  (+ 1 (random (- frontera 2))))

;///////////////
;// FUNCIONES //
;///////////////

; Función para crear la matriz del laberinto
(defun inicializar-matriz-laberinto (n m)
  (crear-filas n m))

; Función recursiva para crear las filas
(defun crear-filas (n m)
  (cond ((zerop n) nil)
        (t (cons (crear-columna m) (crear-filas (1- n) m)))))

; Función recursiva para crear una fila con columnas
(defun crear-columna (m)
  (cond ((zerop m) nil)
        (t (cons 'paret (crear-columna (1- m))))))

; Función para obtener un elemento en (fila, columna)
(defun obtener-elemento (matriz fila columna)
  (cond
    ((zerop fila) (obtener-columna (car matriz) columna))
    (t (obtener-elemento (cdr matriz) (1- fila) columna))))

; Función auxiliar para obtener un elemento en una columna
(defun obtener-columna (fila columna)
  (cond
    ((zerop columna) (car fila))
    (t (obtener-columna (cdr fila) (1- columna)))))

; Función para establecer un elemento en (fila, columna) sin setf
(defun establecer-elemento (matriz fila columna valor)
  (establecer-fila matriz fila columna valor 0))

; Función recursiva para establecer una fila
(defun establecer-fila (matriz fila columna valor current-fila)
  (cond
    ((null matriz) nil)
    ((= current-fila fila)
     (cons (establecer-columna (car matriz) columna valor 0)
           (establecer-fila (cdr matriz) fila columna valor (1+ current-fila))))
    (t
     (cons (car matriz)
           (establecer-fila (cdr matriz) fila columna valor (1+ current-fila))))))

; Función recursiva para establecer una columna
(defun establecer-columna (fila columna valor current-columna)
  (cond
    ((null fila) nil)
    ((= current-columna columna)
     (cons valor (establecer-columna (cdr fila) columna valor (1+ current-columna))))
    (t
     (cons (car fila) (establecer-columna (cdr fila) columna valor (1+ current-columna))))))

; Función proporcionada por el profesor (no usada por ahora, pero incluida)
(defun tornaprimers (n l)
  (cond ((= n 0) nil)
        (t (cons (car l)
                 (tornaprimers (- n 1) (cdr l))))))
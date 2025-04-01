; Función principal para generar el laberinto y probar establecer/obtener
(defun inicio (&optional (n 25) (m 25))
  (let ((laberinto (inicializar-matriz-laberinto n m)))
    ; Establecemos algunos valores
    (let ((laberinto-modificado 
           (establecer-elemento 
            (establecer-elemento laberinto 0 0 'entrada) ; Posición (0, 0)
            24 24 'sortida))) ; Posición (24, 24)
      ; Obtenemos y mostramos los valores para verificar
      (format t "Posicion (0, 0): ~a~%" (obtener-elemento laberinto-modificado 0 0))
      (format t "Posicion (24, 12): ~a~%" (obtener-elemento laberinto-modificado 24 12))
      (format t "Posicion (1, 1): ~a~%" (obtener-elemento laberinto-modificado 1 1))
      ))) ; Retornamos la matriz modificada

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
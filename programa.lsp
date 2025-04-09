; Función principal para generar el laberinto
(defun inicio (nom-fichero &optional (n 25) (m 25))
    (let ((laberinto (inicializar-matriz-laberinto n m)))
      ; Generamos la entrada aleatoria
      (let ((resultado (generar-entrada laberinto n m)))
        (let ((laberinto-con-entrada (car resultado))
              (casilla-actual (car (cdr resultado))))

          ; Ejecutamos el DFS para generar caminos
          (let ((laberinto-con-caminos (generar-dfs laberinto-con-entrada casilla-actual n m)))
            
            ; Añadimos la salida aleatoria
            (let ((laberinto-final (generar-salida laberinto-con-caminos n m)))
              ; Escribimos el laberinto en el archivo
              (escribir-en-fichero laberinto-final nom-fichero n m)))))))

; Función para generar la entrada aleatoria evitando los bordes
(defun generar-entrada (matriz n m)
  (let ((fila-entrada (numero-aleatorio n))
        (columna-entrada (numero-aleatorio m)))
    (let ((laberinto-modificado (establecer-elemento matriz fila-entrada columna-entrada 'entrada)))
      (list laberinto-modificado (list fila-entrada columna-entrada))))) ; Retorna matriz y casilla actual

; Función DFS recursiva para generar caminos
(defun generar-dfs (matriz casilla-actual n m)
  (let ((direcciones (mezclar-direcciones (list (list 0 1) (list 0 -1) (list 1 0) (list -1 0)))))
    (explorar-adyacentes matriz casilla-actual direcciones n m)))

; Función para explorar las casillas adyacentes (corregida)
(defun explorar-adyacentes (matriz casilla-actual direcciones n m)
  (cond ((null direcciones) matriz) ; Si no hay más direcciones, retornamos la matriz
        (t (let ((dir (car direcciones)))
             (let ((nueva-fila (+ (car casilla-actual) (car dir)))
                   (nueva-columna (+ (car (cdr casilla-actual)) (car (cdr dir)))))
               (cond ((and (posicion-valida nueva-fila nueva-columna n m)
                           (eq (obtener-elemento matriz nueva-fila nueva-columna) 'paret)
                           (unica-conexion matriz nueva-fila nueva-columna casilla-actual))
                      ; Marcamos como camino y exploramos desde ahí
                      (let ((matriz-con-camino 
                             (establecer-elemento matriz nueva-fila nueva-columna 'cami)))
                        ; Exploramos desde la nueva posición y luego continuamos con las demás direcciones
                        (let ((matriz-actualizada (generar-dfs matriz-con-camino (list nueva-fila nueva-columna) n m)))
                          (explorar-adyacentes matriz-actualizada casilla-actual (cdr direcciones) n m))))
                     (t (explorar-adyacentes matriz casilla-actual (cdr direcciones) n m))))))))

; Función para generar la salida aleatoria (Punto 5)
(defun generar-salida (matriz n m)
  (let ((caminos (encontrar-caminos matriz 0 0 n m nil)))
    (cond ((null caminos) matriz) ; Si no hay caminos, retornamos sin cambios
          (t (let ((indice (random (longitud-lista caminos))))
               (let ((casilla-salida (obtener-elemento-lista caminos indice)))
                 (establecer-elemento matriz (car casilla-salida) (car (cdr casilla-salida)) 'sortida)))))))

; Función para encontrar todas las casillas 'cami
(defun encontrar-caminos (matriz fila columna n m caminos)
  (cond ((>= fila n) caminos) ; Si terminamos las filas, retornamos los caminos
        ((>= columna m) (encontrar-caminos matriz (+ fila 1) 0 n m caminos)) ; Siguiente fila
        ((eq (obtener-elemento matriz fila columna) 'cami)
         (encontrar-caminos matriz fila (+ columna 1) n m 
                            (cons (list fila columna) caminos))) ; Añadimos el camino
        (t (encontrar-caminos matriz fila (+ columna 1) n m caminos)))) ; Seguimos buscando

; Función para mezclar las direcciones aleatoriamente
(defun mezclar-direcciones (direcciones)
  (cond ((null direcciones) nil)
        (t (let ((indice (random (longitud-lista direcciones))))
             (let ((dir-elegida (obtener-elemento-lista direcciones indice)))
               (cons dir-elegida (mezclar-direcciones (eliminar-elemento-lista direcciones indice))))))))

; Función para verificar si una posición es válida (solo 1 a 23)
(defun posicion-valida (fila columna n m)
  (and (> fila 0) (< fila (- n 1)) (> columna 0) (< columna (- m 1))))

; Función para verificar si solo hay una conexión (la casilla actual)
(defun unica-conexion (matriz fila columna casilla-actual)
  (let ((vecinos (contar-vecinos-camino matriz fila columna)))
    (= vecinos 1)))

; Función para contar vecinos que son 'cami o 'entrada
(defun contar-vecinos-camino (matriz fila columna)
  (let ((direcciones (list (list 0 1) (list 0 -1) (list 1 0) (list -1 0))))
    (contar-vecinos-recursivo matriz fila columna direcciones 0)))

; Función recursiva para contar vecinos
(defun contar-vecinos-recursivo (matriz fila columna direcciones conteo)
  (cond ((null direcciones) conteo)
        (t (let ((nueva-fila (+ fila (car (car direcciones))))
                 (nueva-columna (+ columna (car (cdr (car direcciones))))))
             (cond ((and (posicion-valida nueva-fila nueva-columna (longitud-lista matriz) (longitud-lista (car matriz)))
                         (miembro (obtener-elemento matriz nueva-fila nueva-columna) (list 'cami 'entrada)))
                    (contar-vecinos-recursivo matriz fila columna (cdr direcciones) (+ conteo 1)))
                   (t (contar-vecinos-recursivo matriz fila columna (cdr direcciones) conteo)))))))

; Función para verificar si un elemento está en una lista
(defun miembro (elemento lista)
  (cond ((null lista) nil)
        ((eq elemento (car lista)) t)
        (t (miembro elemento (cdr lista)))))

; Función para obtener la longitud de una lista
(defun longitud-lista (lista)
  (cond ((null lista) 0)
        (t (+ 1 (longitud-lista (cdr lista))))))

; Función para obtener un elemento de una lista por índice
(defun obtener-elemento-lista (lista indice)
  (cond ((zerop indice) (car lista))
        (t (obtener-elemento-lista (cdr lista) (- indice 1)))))

; Función para eliminar un elemento de una lista por índice
(defun eliminar-elemento-lista (lista indice)
  (cond ((zerop indice) (cdr lista))
        (t (cons (car lista) (eliminar-elemento-lista (cdr lista) (- indice 1))))))

; Función para generar un número aleatorio entre 1 y frontera-2 (evita bordes)
(defun numero-aleatorio (frontera)
  (+ 1 (random (- frontera 2))))


; Función para escribir el laberinto en un archivo
(defun escribir-en-fichero (matriz nom-fichero n m)
  (let ((out (open nom-fichero :direction :output :if-exists :supersede)))
    (escribir-filas matriz out 0 n m)
    (close out)))

; Función auxiliar recursiva para escribir las filas
(defun escribir-filas (matriz out fila n m)
  (cond ((= fila n) nil) ; Si hemos terminado todas las filas, terminamos
        (t (escribir-columnas matriz out fila 0 m) ; Escribimos la fila actual
           (write-char #\newline out) ; Añadimos salto de línea
           (escribir-filas matriz out (+ fila 1) n m)))) ; Pasamos a la siguiente fila

; Función auxiliar recursiva para escribir las columnas de una fila
(defun escribir-columnas (matriz out fila columna m)
  (cond ((= columna m) nil) ; Si hemos terminado todas las columnas, terminamos
        (t (let ((valor (obtener-elemento matriz fila columna)))
             (cond ((eq valor 'paret) (write-char #\# out))
                   ((eq valor 'cami) (write-char #\. out))
                   ((eq valor 'entrada) (write-char #\e out))
                   ((eq valor 'sortida) (write-char #\s out))))
           (escribir-columnas matriz out fila (+ columna 1) m)))) ; Pasamos a la siguiente columna


;///////////////
;// FUNCIONES BASE //
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
; Función principal para generar el laberinto y escribirlo en un fichero
(defun genera (nom-fitxer &optional (n 25) (m 25))
    (let ((laberinto (inicializar-matriz-laberinto n m)))
      ; Generamos la entrada aleatoria y guardamos la casilla de entrada
      (let ((resultado (generar-entrada laberinto n m)))
        (let ((laberinto-con-entrada (car resultado))
              (casilla-actual (car (cdr resultado))))
          ; Ejecutamos el DFS para generar caminos
          (let ((laberinto-con-caminos (generar-dfs laberinto-con-entrada casilla-actual n m)))
            ; Añadimos la salida aleatoria
            (let ((laberinto-final (generar-salida laberinto-con-caminos n m)))
              ; Escribimos el laberinto en el archivo
              (escribir-en-fichero laberinto-final nom-fitxer n m)
              ; Preguntamos al usuario si quiere generar otro laberinto
              (let ((respuesta (preguntar-usuario "Desea regenerar el Laberinto?")))
                (cond ((eq respuesta 'yes)
                       (genera nom-fitxer n m)) ; Generamos otro laberinto
                      ((eq respuesta 'no)
                       (escribir-por-pantalla "Generacion de laberintos finalizada correctamente."))
                      (t
                       (escribir-por-pantalla "Respuesta no valida. Finalizando generacion."))))))))))


; Función para generar la entrada aleatoria evitando los bordes
(defun generar-entrada (matriz n m)
  ; selecciona una fila y columna aleatoria
  (let ((fila-entrada (numero-aleatorio n))
        (columna-entrada (numero-aleatorio m)))
        ;establece el valor 'entrada en la casilla aleatoria
    (let ((laberinto-modificado (establecer-elemento matriz fila-entrada columna-entrada 'entrada)))
      (list laberinto-modificado (list fila-entrada columna-entrada))))) ; Retorna matriz y casilla actual

; Función DFS recursiva para generar caminos
(defun generar-dfs (matriz casilla-actual n m)
  ; genera una lista con las 4 posiciones adyacentes en orden aleatorio
  (let ((direcciones (mezclar-direcciones (list (list 0 1) (list 0 -1) (list 1 0) (list -1 0)))))
    (explorar-adyacentes matriz casilla-actual direcciones n m)))

; Función para explorar las casillas adyacentes
(defun explorar-adyacentes (matriz casilla-actual direcciones n m)
  (cond ((null direcciones) matriz) ; Si no hay más direcciones, retornamos la matriz
        (t (let ((dir (car direcciones))) ; guarda la primera direccion
              ; calculamos la nueva casilla a partir de la casilla actual y la direccion aleatoria
             (let ((nueva-fila (+ (car casilla-actual) (car dir)))
                   (nueva-columna (+ (car (cdr casilla-actual)) (car (cdr dir)))))
               (cond ((and (posicion-valida nueva-fila nueva-columna n m); [0-23]
                           (eq (obtener-elemento matriz nueva-fila nueva-columna) 'paret); es pared
                           (unica-conexion matriz nueva-fila nueva-columna casilla-actual)); de todas las vecinas la unica entrada o camino es la actual
                      ; Marcamos como camino y exploramos desde ahí
                      (let ((matriz-con-camino 
                             (establecer-elemento matriz nueva-fila nueva-columna 'cami)))
                        ; Exploramos desde la nueva posición y luego continuamos con las demás direcciones
                        (let ((matriz-actualizada (generar-dfs matriz-con-camino (list nueva-fila nueva-columna) n m)))
                          (explorar-adyacentes matriz-actualizada casilla-actual (cdr direcciones) n m))))
                     ;si no se cumplen las codinciones explora con la siguiente direcci
                     (t (explorar-adyacentes matriz casilla-actual (cdr direcciones) n m))))))))

; Función para generar la salida aleatoria
(defun generar-salida (matriz n m)
  ; lista con todas las casillas 'cami
  (let ((caminos (encontrar-caminos matriz 0 0 n m nil)))
    (cond ((null caminos) matriz) ; Si no hay caminos, retornamos sin cambios
          ; selecciona una casilla aleatoria
          (t (let ((indice (random (longitud-lista caminos))))
               (let ((casilla-salida (obtener-elemento-lista caminos indice)))
                ; retorna la matriz con una casilla con valor 'sortida
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
              ; seleccionamos una dir aleatoria y la metemos al principio y llamamos otra vez a la funcion sin esta dir
             (let ((dir-elegida (obtener-elemento-lista direcciones indice)))
               (cons dir-elegida (mezclar-direcciones (eliminar-elemento-lista direcciones indice))))))))

; Función para verificar si una posición es válida (solo 1 a 23)
(defun posicion-valida (fila columna n m)
  (and (> fila 0) (< fila (- n 1)) (> columna 0) (< columna (- m 1))))

; Función para verificar si solo hay una conexión (la casilla actual)
(defun unica-conexion (matriz fila columna casilla-actual)
  (let ((vecinos (contar-vecinos-camino matriz fila columna)))
    (= vecinos 1))) ; retorna t si solo hay un vecino

; Función para contar vecinos que son 'cami o 'entrada
(defun contar-vecinos-camino (matriz fila columna)
  (let ((direcciones (list (list 0 1) (list 0 -1) (list 1 0) (list -1 0))))
    (contar-vecinos-recursivo matriz fila columna direcciones 0)))

; Función recursiva para contar vecinos
(defun contar-vecinos-recursivo (matriz fila columna direcciones conteo)
  (cond ((null direcciones) conteo); si no hay direcciones retornamos conteo
        ;calcula la nueva posicion
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


;///////////////////////////////////////////////////////////////////////////////////
;/////////////////////// FUNCIONES PARA EXPLORAR EL LABERINTO //////////////////////
;///////////////////////////////////////////////////////////////////////////////////
; Función para obtener el nombre del usuario desde la consola
(defun obtener-nombre-usuario ()
  (escribir-por-pantalla "Por favor, introduce tu nombre:")
  (let ((nombre (read))) ; Leemos la entrada del usuario
    (string nombre))) ; Convertimos el símbolo a cadena

; Función para comparar dos strings alfabéticamente
(defun string-menor (s1 s2)
  (string< s1 s2))

; Función auxiliar recursiva para dividir la línea
(defun split-linea-aux (chars current-token tokens)
  (cond ((null chars)
         (cond ((null current-token) (reverse tokens))
               (t (reverse (cons (coerce (reverse current-token) 'string) tokens)))))
        ((char= (car chars) #\space)
         (cond ((null current-token)
                (split-linea-aux (cdr chars) nil tokens))
               (t (split-linea-aux (cdr chars) nil (cons (coerce (reverse current-token) 'string) tokens)))))
        (t (split-linea-aux (cdr chars) (cons (car chars) current-token) tokens))))

; Función auxiliar para leer una línea del archivo usando read-char
(defun leer-linea-con-char (in caracteres)
  (let ((char (read-char in nil nil))) ; Leemos un carácter del archivo
    (cond ((null char) ; Si llegamos al final del archivo
           (cond ((null caracteres) nil) ; Si no hay caracteres acumulados, terminamos
                 (t (reverse caracteres)))) ; Devolvemos la última línea acumulada
          ((eq char #\newline) ; Si encontramos un salto de línea
           (reverse caracteres)) ; Devolvemos la línea acumulada
          (t (leer-linea-con-char in (cons char caracteres)))))) ; Acumulamos el carácter y seguimos

; Función auxiliar para escribir una lista de entradas en el archivo
(defun escribir-entradas (out entradas)
  (cond ((null entradas) nil)
        (t (format out "~a ~a ~a~%" (car (car entradas)) (cadr (car entradas)) (caddr (car entradas)))
           (escribir-entradas out (cdr entradas)))))

; Función auxiliar para dibujar un rectángulo relleno usando move y draw
(defun dibujar-rectangulo (x y ancho alto)
  (let ((y-fin (+ y alto))) ; Calculamos la coordenada y final
    (do ((current-y y (+ current-y 1))) ; Iteramos desde y hasta y + alto
        ((>= current-y y-fin)) ; Condición de parada
      (move x current-y) ; Nos movemos al inicio de la línea
      (draw (+ x ancho) current-y)))) ; Dibujamos una línea horizontal

; Función para dividir una línea en tokens (separados por espacios)
(defun split-linea (linea)
  (split-linea-aux linea nil nil))

; Función auxiliar para leer todas las líneas del archivo usando read-char
(defun leer-todas-las-lineas (in lineas)
  (let ((linea (leer-linea-con-char in nil))) ; Leemos una línea carácter por carácter
    (cond ((null linea) lineas) ; Si no hay más líneas, devolvemos las líneas en orden
          (t (leer-todas-las-lineas in (cons linea lineas))))))

; Función para parsear una línea del archivo Resultados.txt
(defun parsear-linea-resultados (linea)
  (let ((tokens (split-linea linea)))
    (list (car tokens) ; nombre-laberinto
          (cadr tokens) ; nombre-usuario
          (parse-integer (caddr tokens))))) ; puntuacion (convertida a número)

; Función auxiliar recursiva para parsear los dígitos de la cadena
(defun parse-digits (str index len result)
  (cond ((= index len) result) ; Si hemos terminado, devolvemos el resultado
        (t (let ((digit (- (char-code (char str index)) (char-code #\0))))
             (cond ((or (< digit 0) (> digit 9))
                    (error "Cadena inválida: ~a no es un número entero" str))
                   (t (parse-digits str
                                    (+ index 1)
                                    len
                                    (+ (* result 10) digit))))))))

; Función principal para convertir una cadena en un entero
(defun parse-integer (str)
  (let ((len (length str)))
    (cond ((= len 0) (error "Cadena vacía no es un número entero"))
          ((char= (char str 0) #\-) ; Si es un número negativo
           (* -1 (parse-digits str 1 len 0)))
          (t (parse-digits str 0 len 0)))))

; Predicado para ordenar entradas: primero por nombre-laberinto, luego por puntuación
(defun comparar-entradas (entrada1 entrada2)
  (let ((lab1 (car entrada1))
        (lab2 (car entrada2))
        (puntuacion1 (caddr entrada1))
        (puntuacion2 (caddr entrada2)))
    (cond ((string-menor lab1 lab2) t)
          ((string-menor lab2 lab1) nil)
          (t (> puntuacion1 puntuacion2)))))

; Función para leer todas las entradas del archivo Resultados.txt
(defun leer-entradas-resultados ()
  (let ((in (open "Resultados.txt" :direction :input :if-does-not-exist nil)))
    (cond ((null in) nil)
          (t (let ((lineas (leer-todas-las-lineas in nil)))
               (close in)
               (mapcar #'parsear-linea-resultados (reverse lineas))))))) ; Invertimos las líneas

; Función auxiliar recursiva para cargar las columnas de una fila desde una línea de texto
(defun cargar-columnas (matriz fila linea columna m)
  (cond ((= columna m) matriz) ; Si hemos terminado las columnas, retornamos la matriz
        (t (let ((caracter (nth columna linea))) ; Obtenemos el carácter en la posición actual
             (let ((valor (case caracter
                            (#\# 'paret)
                            (#\. 'cami)
                            (#\e 'entrada)
                            (#\s 'sortida))))
               (cargar-columnas (establecer-elemento matriz fila columna valor) fila linea (+ columna 1) m))))))

; Función auxiliar recursiva para pintar las columnas de una fila
(defun pintar-columnas (matriz fila columna m ancho-casilla alto-casilla posicion-actual)
  (cond ((= columna m) nil) ; Si hemos terminado las columnas, terminamos
        (t (let ((valor (obtener-elemento matriz fila columna))
                 (x (* columna ancho-casilla))
                 (y (* fila alto-casilla)))
             (cond ((eq valor 'paret) 
                    (color 0 0 0)) ; Negro para paredes
                   ((eq valor 'cami) 
                    (color 255 255 255)) ; Blanco para caminos
                   ((eq valor 'entrada) 
                    (color 0 0 255)) ; Azul para la entrada
                   ((eq valor 'sortida) 
                    (color 255 0 0))) ; Rojo para la salida
             (dibujar-rectangulo x y ancho-casilla alto-casilla)
             (cond ((and (= fila (car posicion-actual)) (= columna (car (cdr posicion-actual))))
                    (color 0 255 0) ; Verde para el jugador
                    (dibujar-rectangulo x y ancho-casilla alto-casilla))))
           (pintar-columnas matriz fila (+ columna 1) m ancho-casilla alto-casilla posicion-actual))))

; Función para intentar mover al jugador según la tecla pulsada
(defun mover-jugador (matriz posicion-actual tecla n m)
  (let ((fila-actual (car posicion-actual))
        (columna-actual (car (cdr posicion-actual))))
    (cond ((or (eq tecla 65) (eq tecla 97) (eq tecla 331)) ; Izquierda (A, a, flecha izquierda)
           (let ((nueva-columna (- columna-actual 1)))
             (cond ((and (>= nueva-columna 0)
                         (member (obtener-elemento matriz fila-actual nueva-columna) '(cami entrada sortida)))
                    (list fila-actual nueva-columna)) ; Movemos si es válido
                   (t posicion-actual))))
          ((or (eq tecla 87) (eq tecla 119) (eq tecla 328)) ; Arriba (W, w, flecha arriba) - Aumentar fila
           (let ((nueva-fila (+ fila-actual 1))) ; Invertimos: subir en pantalla = aumentar fila
             (cond ((and (< nueva-fila n) ; Cambiamos la condición para permitir moverse hacia abajo en la matriz
                         (member (obtener-elemento matriz nueva-fila columna-actual) '(cami entrada sortida)))
                    (list nueva-fila columna-actual))
                   (t posicion-actual))))
          ((or (eq tecla 68) (eq tecla 100) (eq tecla 333)) ; Derecha (D, d, flecha derecha)
           (let ((nueva-columna (+ columna-actual 1)))
             (cond ((and (< nueva-columna m)
                         (member (obtener-elemento matriz fila-actual nueva-columna) '(cami entrada sortida)))
                    (list fila-actual nueva-columna))
                   (t posicion-actual))))
          ((or (eq tecla 83) (eq tecla 115) (eq tecla 336)) ; Abajo (S, s, flecha abajo) - Disminuir fila
           (let ((nueva-fila (- fila-actual 1))) ; Invertimos: bajar en pantalla = disminuir fila
             (cond ((and (>= nueva-fila 0) ; Cambiamos la condición para permitir moverse hacia arriba en la matriz
                         (member (obtener-elemento matriz nueva-fila columna-actual) '(cami entrada sortida)))
                    (list nueva-fila columna-actual))
                   (t posicion-actual))))
          (t posicion-actual)))) ; Si la tecla no es válida, no movemos

; Función auxiliar para encontrar la posición de la entrada
(defun encontrar-entrada (matriz fila columna n m)
  (cond ((>= fila n) nil) ; Si terminamos las filas sin encontrar, retornamos nil
        ((>= columna m) (encontrar-entrada matriz (+ fila 1) 0 n m)) ; Siguiente fila
        ((eq (obtener-elemento matriz fila columna) 'entrada) 
         (list fila columna)) ; Devolvemos la posición de la entrada
        (t (encontrar-entrada matriz fila (+ columna 1) n m)))) ; Seguimos buscando

; Función auxiliar recursiva para cargar las filas desde la lista de líneas
(defun cargar-filas (matriz lineas fila n m)
  (cond ((= fila n) matriz) ; Si hemos terminado todas las filas, retornamos la matriz
        (t (let ((linea (car lineas)))
             (cargar-filas (cargar-columnas matriz fila linea 0 m) (cdr lineas) (+ fila 1) n m)))))

; Función auxiliar recursiva para pintar las filas
(defun pintar-filas (matriz fila n m ancho-casilla alto-casilla posicion-actual)
  (cond ((= fila n) nil) ; Si hemos terminado las filas, terminamos
        (t (pintar-columnas matriz fila 0 m ancho-casilla alto-casilla posicion-actual)
           (pintar-filas matriz (+ fila 1) n m ancho-casilla alto-casilla posicion-actual))))

; Función para cargar el laberinto desde un archivo de texto y detectar sus dimensiones
(defun cargar-laberinto (nom-fitxer)
  (let ((in (open nom-fitxer :direction :input)))
    (cond ((null in) (error "No se pudo abrir el archivo ~a" nom-fitxer)) ; Manejo básico de errores
          (t (let ((lineas (leer-todas-las-lineas in nil)))
               (close in) ; Cerramos el archivo después de leer
               (let ((n (longitud-lista lineas)) ; Número de filas
                     (m (length (car lineas)))) ; Longitud de la primera línea (columnas)
                 (let ((matriz (inicializar-matriz-laberinto n m)))
                   (let ((matriz-actualizada (cargar-filas matriz lineas 0 n m)))
                     (list matriz-actualizada n m)))))))))

; Función para pintar el laberinto usando las funciones gráficas disponibles
(defun pintar-laberinto (matriz n m posicion-actual)
  (let ((ancho-casilla (floor (/ 640 n))) ; 640 px dividido entre el número de filas, redondeado a entero
        (alto-casilla (floor (/ 375 m)))) ; 375 px dividido entre el número de columnas, redondeado a entero
    (cls) ; Limpiamos la pantalla antes de dibujar
    (pintar-filas matriz 0 n m ancho-casilla alto-casilla posicion-actual)))

; Función para escribir los resultados de la partida en Resultados.txt, ordenados
(defun escribir-partida (nom-fitxer nombre-usuario puntuacion)
  (let ((entradas (leer-entradas-resultados)))
    (let ((entradas-actualizadas (cons (list nom-fitxer nombre-usuario puntuacion) entradas)))
      (let ((entradas-ordenadas (sort entradas-actualizadas #'comparar-entradas)))
        (let ((out (open "Resultados.txt"
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)))
          (escribir-entradas out entradas-ordenadas)
          (close out))))))


; Función para finalizar el juego, simplificada para no mostrar las puntuaciones
(defun finalizar-juego (motivo pasos n m nom-fitxer nombre-usuario)
  (cls) ; Limpiamos la pantalla para borrar el tablero
  (cond ((eq motivo 'llegada-a-salida)
         (let ((puntuacion (* (/ 1000 (+ pasos 1)) (* n m))))
           (escribir-por-pantalla "Felicidades! Has llegado a la salida.")
           (format t "Pasos realizados: ~a~%" pasos)
           (format t "Puntuacion: ~a~%" (round puntuacion))
           (escribir-partida nom-fitxer nombre-usuario (round puntuacion))
           (list 'finalizado puntuacion))) ; Retornamos una lista con el estado y la puntuación
        ((eq motivo 'esc)
         (escribir-por-pantalla "Finalizacion de la Partida por parte del Usuario")
         (list 'esc nil))
        (t
         (error "Motivo de finalizacion no valido: ~a" motivo))))

; Función para manejar el bucle de juego y la interacción del usuario
(defun jugar-laberinto (matriz n m posicion-actual pasos nom-fitxer nombre-usuario)
  (pintar-laberinto matriz n m posicion-actual) ; Dibujamos el laberinto inicial
  (let ((tecla (get-key))) ; Detectamos la tecla pulsada
    (cond ((eq tecla 27) ; Si es ESC (código 27), terminamos
           (finalizar-juego 'esc pasos n m nom-fitxer nombre-usuario))
          (t (let ((nueva-posicion (mover-jugador matriz posicion-actual tecla n m)))
               (let ((nuevos-pasos (cond ((equal posicion-actual nueva-posicion) pasos) ; Si la posición no cambió, no incrementamos
                                         (t (+ pasos 1)))))
                 (cond ((eq (obtener-elemento matriz (car nueva-posicion) (car (cdr nueva-posicion))) 'sortida)
                        (finalizar-juego 'llegada-a-salida nuevos-pasos n m nom-fitxer nombre-usuario)) ; Si llegamos a la salida, terminamos
                       (t (jugar-laberinto matriz n m nueva-posicion nuevos-pasos nom-fitxer nombre-usuario))))))))) ; Si no, continuamos

; Función auxiliar para filtrar las entradas por nombre de laberinto
(defun filtrar-por-laberinto (entradas nom-fitxer)
  (cond ((null entradas) nil)
        ((string= (car (car entradas)) nom-fitxer)
         (cons (car entradas) (filtrar-por-laberinto (cdr entradas) nom-fitxer)))
        (t (filtrar-por-laberinto (cdr entradas) nom-fitxer))))

; Función auxiliar para tomar hasta las primeras 10 entradas
(defun tomar-10-primeras (entradas contador)
  (cond ((null entradas) nil)
        ((= contador 10) nil)
        (t (cons (car entradas) (tomar-10-primeras (cdr entradas) (+ contador 1))))))

; Función auxiliar para mostrar las entradas filtradas
(defun mostrar-mejores-puntuaciones (entradas posicion)
  (cond ((null entradas) nil)
        (t (format t "~a. ~a - ~a puntos~%" posicion (cadr (car entradas)) (caddr (car entradas)))
           (mostrar-mejores-puntuaciones (cdr entradas) (+ posicion 1)))))                       

; Función para explorar el laberinto con opción de repetir
(defun explora (nom-fitxer)
  (let ((nombre-usuario (obtener-nombre-usuario)))
    (format t "Hola, ~a! Vamos a explorar el laberinto en ~a~%" nombre-usuario nom-fitxer)
    (let ((resultado-carga (cargar-laberinto nom-fitxer)))
      (let ((laberinto (car resultado-carga))
            (n (car (cdr resultado-carga)))
            (m (car (cdr (cdr resultado-carga)))))
        (let ((posicion-inicial (encontrar-entrada laberinto 0 0 n m)))
          (let ((resultado (jugar-laberinto laberinto n m posicion-inicial 0 nom-fitxer nombre-usuario)))
            (when (eq (car resultado) 'finalizado) ; Mostramos las puntuaciones solo si llegó a la salida
              (let ((entradas (leer-entradas-resultados)))
                (let ((entradas-filtradas (filtrar-por-laberinto entradas nom-fitxer)))
                  (let ((mejores-10 (tomar-10-primeras entradas-filtradas 0))) ; Eliminamos el reverse
                    (format t "~%Mejores puntuaciones para ~a:~%" nom-fitxer)
                    (cond ((null mejores-10)
                           (escribir-por-pantalla "Todavia no hay puntuaciones para este laberinto."))
                          (t (mostrar-mejores-puntuaciones mejores-10 1)))))))
            (let ((respuesta (preguntar-usuario "Desea explorar este Laberinto de nuevo?")))
              (cond ((eq respuesta 'yes)
                     (explora nom-fitxer))
                    ((eq respuesta 'no)
                     (escribir-por-pantalla "Exploracion finalizada. Gracias por jugar!"))
                    (t
                     (escribir-por-pantalla "Respuesta no valida. Finalizando exploracion."))))))))))


;/////////////////////////////////////
;// FUNCIONES BASE (GENERAR MATRIZ) //
;/////////////////////////////////////

; Función para crear la matriz del laberinto
(defun inicializar-matriz-laberinto (n m)
  (crear-filas n m))

; Función recursiva para crear las filas
(defun crear-filas (n m)
  (cond ((zerop n) nil)
        (t (cons (crear-columna m) (crear-filas (- n 1) m)))))

; Función recursiva para crear una fila con columnas
(defun crear-columna (m)
  (cond ((zerop m) nil)
        (t (cons 'paret (crear-columna (- m 1))))))

; Función para obtener un elemento en (fila, columna)
(defun obtener-elemento (matriz fila columna)
  (cond ((zerop fila) (obtener-columna (car matriz) columna))
        (t (obtener-elemento (cdr matriz) (- fila 1) columna))))

; Función auxiliar para obtener un elemento en una columna
(defun obtener-columna (fila columna)
  (cond ((zerop columna) (car fila))
        (t (obtener-columna (cdr fila) (- columna 1)))))

; Función para establecer un elemento en (fila, columna) sin setf
(defun establecer-elemento (matriz fila columna valor)
  (establecer-fila matriz fila columna valor 0))

; Función recursiva para establecer una fila
(defun establecer-fila (matriz fila columna valor current-fila)
  (cond ((null matriz) nil)
        ((= current-fila fila)
         (cons (establecer-columna (car matriz) columna valor 0)
               (establecer-fila (cdr matriz) fila columna valor (+ current-fila 1))))
        (t (cons (car matriz)
                 (establecer-fila (cdr matriz) fila columna valor (+ current-fila 1))))))

; Función recursiva para establecer una columna
(defun establecer-columna (fila columna valor current-columna)
  (cond ((null fila) nil)
        ((= current-columna columna)
         (cons valor (establecer-columna (cdr fila) columna valor (+ current-columna 1))))
        (t (cons (car fila) (establecer-columna (cdr fila) columna valor (+ current-columna 1))))))

; Definimos longitud-lista como length
(defun longitud-lista (lista)
  (length lista))

; Función para escribir un mensaje por pantalla
(defun escribir-por-pantalla (mensaje)
  (format t "~a~%" mensaje))

; Función para preguntar al usuario y detectar si responde Y/y, N/n, o ninguna de las dos
(defun preguntar-usuario (mensaje)
  (format t "~a: (Y/N) " mensaje) ; Mostramos el mensaje seguido de : (Y/N)
  (let ((respuesta (string (read)))) ; Leemos la entrada y la convertimos a cadena
    (cond ((or (string= respuesta "Y") (string= respuesta "y")) 'yes)
          ((or (string= respuesta "N") (string= respuesta "n")) 'no)
          (t nil))))
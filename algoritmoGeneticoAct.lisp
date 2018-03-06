; Algoritmo genetico
; Autor: G. Alvarez
; Fecha: agosto 30 de 2017
; Completado por: Daniel Cano Salgado
; 2017 - 2 / Inteligencia Artificial



;Elimina la mitad de los individuos, los menos aptos YA
(defun elimInd (gen tGen)
  (cond ((> (length gen) (/ tGen 2)) (cons (car gen) ( elimInd (cdr gen) tGen)))
  )
)

; Algoritmo para cruzar individuos y completar la generacion YA
(defun cruzarInd (individuos)
  (setq r (random (length individuos)))
  (setq q (random (length individuos)))
  (cruzar  (nth r individuos) (nth q individuos))
)

; Cruza 2 individuos YA
(defun cruzar (individuo1 individuo2)
  (setq corte (random (length individuo1)))
  (cons (append (subseq individuo2 0 corte) (subseq individuo1 corte (length individuo1))) (list (append (subseq individuo1 0 corte) (subseq individuo2 corte (length individuo1)))))
)

; Genera un tipo de cambio para la mutacion de manera aleatoria YA
(defun randomType ()
  (let ((x '(N C E I)))
    (nth (random (length x)) x)
  )
)

; Se elimina la aptitud actual para comodidad de ejecución YA
(defun quitarApt (individuos)
  (cond((> (length individuos) 0) (cons (car(cdr (car individuos))) (quitarApt (cdr individuos))))
  )
)

; Algoritmo para mutacion de individuo YA
(defun mutarInd (individuos pMut)
  (setq r (random 101))
  (setq q (random 101))
  (setq m (random (length (car individuos))))
  (cond ((and (> r pMut)(> q pMut)) (append(list(cambiar (car individuos) m (giveVal (cons (randomType) (list m)))))
                                    (list(cambiar (cadr individuos) m (giveVal (cons (randomType) (list m)))))))
        ((> r pMut)(append (list(cadr individuos))(list(cambiar (car individuos) m (giveVal (cons (randomType) (list m)))))))
        ((> q pMut)(append (list(car individuos))(list(cambiar (cadr individuos) m (giveVal (cons (randomType) (list m)))))))
        (t individuos)
  )
)

; Se genera la nueva generación de individuos hasta tener tamaño tGen YA
(defun armarNuevaGen ( tGen actuales pMut)
  (cond ((< (length actuales) (+ tGen 1)) (armarNuevaGen tGen (append actuales (mutarInd (cruzarInd actuales) pMut)) pMut))
        ((> (length actuales) tGen) (quitarApt(eliminarExtra tGen (aptitud actuales))))
        (t actuales)
  )
)
; eliminar ultimo elemento YA
(defun quitarUltimo(l)
    (reverse (cdr (reverse l)))
)
; Eliminar sobrantes que salieron en la creación YA
(defun eliminarExtra (tGen individuos)
  (cond ((< tGen (length individuos)) (eliminarExtra tGen (quitarUltimo individuos)))
        (t individuos)
  )
)

; construye una nueva generacion a partir de la que recibe
(defun nuevaGen (tGen gen pMut)
  (let ((siguenVivos (elimInd gen tGen)))
    (aptitud (armarNuevaGen tGen (append siguenvivos (mutarInd (cruzarInd  siguenvivos) pMut)) pMut))))

; determina si el individuo que recibe como parametro es ya una solucion  YA
(defun esSolucion (apt)
  (> apt 90))

; combina los individuos y sus aptitudes para poder ordenarlos YA
(defun mezclar (lista1 lista2)  
  (if (null lista1) 
      nil
      (cons (list (car lista1)(car lista2))
            (mezclar (cdr lista1)(cdr lista2))))
)

; ordena las listas con los individuos y sus correspondientes aptitudes YA
(defun ordenar (ind apt)
  (sort (mezclar apt ind) #'> :key #'first))

;Inserta un valor A en la posicion index de la lista YA
(defun insertar(lista index a)
  (cond ((= index 0) (cons a lista))
        (t(cons (car lista) (insertar (cdr lista)(- index 1) a)))
  )
)
; elimina el valor de la posicion index YA
(defun eliminar (lista index) 
  (cond ((= index 0) (cdr lista))
        (t(cons(car lista)(eliminar (cdr lista) (- index 1))))
  )
)
; Cambia el valor de la posicion index por A en la lista YA
(defun cambiar ( lista index a)
  (cond ((= index 0) (cons a(cdr lista)))
        (t(cons(car lista)(cambiar (cdr lista) (- index 1) a)))
  )
)
; Aplica un cambio a la palabra x YA
(defun change (cambio x)
  (cond ((equal(car cambio) 'I)
          (insertar x (nth 1 cambio) (nth 2 cambio)))
        ((equal(car cambio) 'E)
          (eliminar x (nth 1 cambio)))
        ((equal(car cambio) 'C)
          (cambiar x (nth 1 cambio) (nth 2 cambio)))
        (t x)
  )
)

; Aplica una lista de cambios a la palabra x YA
(defun aply (individuo x)
  (cond ((> (length individuo) 0)(aply (cdr individuo)(change (car individuo) x)))
        (t x)
  )
)
; verifica si 2 letras son iguales, usada mas abajo YA
(defun igualdad ( a b)
  (cond (( equal a b) 0)
        (t 1)
  )
)
; verifica si 2 cadenas son iguales, retorna la cantidad de diferencias YA
(defun compare (x y)
  (cond((= (length x) 0) 0)
        (t (+(igualdad (car x)(car y)) (compare (cdr x)(cdr y))))
  )
)

; funcion de aptitud para la tarea a resolver  YA
(defun fapt (ind)
  (let ((x '(H O L A))(y '(P A L A B R A)))
    (* 100 (/(- (- (length y) (abs(- (length y)(length (aply ind x)))))(compare (aply ind x) y))(length y)))
  )
)

; Calcula la aptitud de cada individuo de la generacion y los retorna ordenados de mayor 
; a menor aptitud  YA
(defun aptitud (gen)
  (ordenar gen (mapcar 'fapt gen)))

; Extrae el mejor individuo de la generacion gen  YA
(defun mejorInd (gen)
  (car (aptitud gen)))

; algoritmo genetico
(defun genetico (tGen gen pMut cGen)
  (let ((mInd (mejorInd gen)))
    (cond ((esSolucion (car mInd)) mInd)
          ((< cGen 0) mInd)
          (t(genetico tGen (quitarApt(nuevaGen tGen  gen pMut)) pMut (- cGen 1)))
    )
  )
)

; Da un valor a la operacion del individuo YA
(defun giveVal (val)
  (let ((x '(H O L A)) (letras '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))
    (cond ((or(equal (car val) 'I) (equal (car val) 'C)) 
            (cons (car val) (cons (random (length x)) (list(getVal(random (length letras)) letras)))))
          ((equal (car val) 'E)
            (cons (car val) (list(random (length x)))))
          (t(list(car val)))
    )
  )
)
; Selecciona el valor del dominio a ubicar en el individuo YA
(defun getVal (number dominio)
  (cond ((=  number 0)(car dominio))
        (t (getVal (- number 1)(cdr dominio)))
  )
)
; construye un individuo aleatorio de acuerdo al problema YA
(defun genInd (a domInd)
  (if(> (length a) 0)
    (cons(giveVal(getVal (random (length domInd)) domInd))(genInd (cdr a) domInd))
  )
)

; construye la primera generacion de acuerdo a los individuos del problema YA
(defun primeraGen (tGen domInd k)
  (mapcar 'genInd (make-list tGen :initial-element (make-list k))(make-list tGen :initial-element domInd))
)

; funcion principal, recube el tamaño de la generacion, la probabilidad de mutacion
; el dominio del individuo y el tamaño del individuo y el tamaño de la generación
; individuoDominio = ((I 0 A)(C 0 A)(E 0)(N))
(defun ppalGen (tGen pMut domInd k cGen)
  (genetico tGen (primeraGen  tGen domInd k) pMut cGen))
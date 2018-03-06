; Algoritmo genetico
; Por: Daniel Cano Salgado
; 2017 - 2 / Inteligencia Artificial
; Es usado para reemplazar el Back Propagation de un perceptr贸n Multicapa 5-2-1


;Elimina la mitad de los individuos, los menos aptos YA
(defun elimInd (gen tGen)
  (cond ((> (length gen) (/ tGen 2)) (cons (car gen) ( elimInd (cdr gen) tGen)))
  )
)

; Algoritmo para cruzar individuos y completar la generacion YA
(defun cruzarInd (individuos)
  (setq r (random (length individuos)))
  (setq q (random (length individuos))) 
  (setq p (cruzar  (car (nth r individuos)) (car (nth q individuos))))
  (setq s (cruzar  (cadr (nth r individuos)) (cadr (nth q individuos))))
  (append (list(append (list(nth 0 p))(list(nth 0 s))))(list(append (list(nth 1 p))(list(nth 1 s)))))
)

; Cruza 2 individuos YA
(defun cruzar (individuo1 individuo2)
  (setq corte (random (length individuo1)))
  (cons (append (subseq individuo2 0 corte) (subseq individuo1 corte (length individuo1))) (list (append (subseq individuo1 0 corte) (subseq individuo2 corte (length individuo1)))))
)


; Se elimina la aptitud actual para comodidad de ejecuci贸n YA
(defun quitarApt (individuos)
  (cond((> (length individuos) 0) (cons (car(cdr (car individuos))) (quitarApt (cdr individuos))))
  )
)

; Algoritmo para mutacion de individuo YA
(defun mutarInd (individuos pMut)
  (setq r (random 101))
  (setq q (random 101))
  (setq m (random (length (caar individuos))))
  (setq n (random (length (cadar individuos))))
  (cond ((and (> r pMut)(> q pMut))
		(append (list(append (list(cambiar (caar individuos) m (cons (randomfull 1.0)(list(randomfull 1.0)))))(list(cambiar (cadar individuos) n (list(randomfull 1.0))))))(list(append(list( cambiar (caadr individuos) m (cons (randomfull 1.0)(list(randomfull 1.0))))) (list(cambiar (cadadr individuos) n (list(randomfull 1.0))))))))
        ((> r pMut)(append (list(append (list(cambiar (caar individuos) m (cons (randomfull 1.0)(list(randomfull 1.0)))))(list(cambiar (cadar individuos) n (list(randomfull 1.0))))))(cdr individuos)))
        ((> q pMut)(append (list (car individuos))(list(append(list( cambiar (caadr individuos) m (cons (randomfull 1.0)(list(randomfull 1.0))))) (list(cambiar (cadadr individuos) n (list(randomfull 1.0))))))))
        (t individuos)
  )
)

; Se genera la nueva generaci贸n de individuos hasta tener tama帽o tGen YA
(defun armarNuevaGen ( tGen actuales pMut)
  (cond ((< (length actuales) (+ tGen 1)) (armarNuevaGen tGen (append actuales (mutarInd (cruzarInd actuales) pMut)) pMut))
        ((> (length actuales) tGen) (quitarApt(eliminarExtra tGen (aptitud actuales))))
        (t  actuales)
  )
)
; eliminar ultimo elemento YA
(defun quitarUltimo(l)
    (reverse (cdr (reverse l)))
)
; Eliminar sobrantes que salieron en la creaci贸n YA
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

; funcion de aptitud para la tarea a resolver  YA
(defun fapt (ind val)
	(setq dataRaw (read-data "Training.csv"))
	(setq data (cleanData dataRaw ()))
	(fapt1 ind val data (length data))
)

(defun fapt1 (ind val data tot)
	(cond ((equal 0 (length data))(/ val tot))
		((equal (cadaar data)(caddr (propagation (caar data) ind 'escalon))) (fapt1 ind (+ val 1)(cdr data) tot))
		(t (fapt1 ind val (cdr data) tot))
  )
)
; Calcula la aptitud de cada individuo de la generacion y los retorna ordenados de mayor 
; a menor aptitud  YA
(defun aptitud (gen)
  (ordenar gen (mapcar 'fapt gen (make-list (length gen) :initial-element 0))))

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
; construye la primera generacion de acuerdo a los individuos del problema YA
(defun primeraGen (k listaGen)
  (cond ((> k 0) (cons (genRed '(5 2 1))(primeraGen (- k 1) listaGen )))
        (t listaGen)
  )
)
; funcion principal, recibe el tama駉 de generaci髇, porcentaje de mutacion, una lista vacia y la cantidad de
; generaciones
(defun ppalGen (tGen pMut listaGen cGen)
  (genetico tGen (primeraGen  tGen listaGen) pMut cGen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Red Neuronal sin Back Propagation
(defun escalon (x)
  (cond((<= x 0.7) 0)
    ((and (> x 0.7)(<= x 1.2)) 1)
    ((and (> x 1.2)(<= x 1.7)) 2)
    (t 3)
  )
)
;; funcion que propaga las entradas hasta la siguiente capa, pasando por las respectivas unidades.
(defun propagationLayer (in m f)
  (cond ((null (car m)) nil)
    (t (cons (funcall f (reduce '+ ( mapcar '* in (mapcar 'car m)))) (propagationLayer in (mapcar 'cdr m) f)))
  )
)
;; funcion que reliza la propagacion hacia adenlante hasta la capa de salida.
(defun propagation (in red f)
  (cond ((null red) in)
    (t (cons in (propagation(propagationLayer (cons 1 in) (car red) f) (cdr red) f)))
  )
)
;;funcion que genera una red dada su configuracion (numero de unidades por capa)
;;Estilo configuracion = ( 5 3 1)
;; 5 entradas, capa oculta de 3 nodos, salida de 1 nodo
(defun genRed(configuracion)
  (cond ((null (cdr configuracion)) nil)
    (t (cons (genMatrizPesos (car configuracion) (cadr configuracion)) (genRed (cdr configuracion))))
  )
)
;; funcion que genera una maptriz representando los pesos entre dos capas.
(defun genMatrizPesos(n m)
  (cond ((eq n -1) nil)
    (t (cons (genVectorPesos m) (genMatrizPesos (- n 1) m)))
  )
)
;;Genera un numero random en el rango [-n,n]
(defun randomfull (n)
  (- (random (1+ (* 2 n))) n)
)
;;funcion que entrega un numero n de pesos aleatorios entre -1 y 1.
(defun genVectorPesos (n)
  (cond ((eq n 0) nil)
    (t (cons (randomfull 1.0) (genVectorPesos(- n 1))))
  )
)


;Lectura de datos del csv
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range(N) (loop for i from 0 below N collect i))

(defun select-column (collection column)
(map 'vector (lambda (x) (aref x column)) collection))

(ql:quickload :cl-ppcre)

(defun read-data(name)
    (with-open-file (file name)
        (let (line)
            (loop while (setf line (read-line file nil)) collect
                  (if (> (length line) 0)
                     (map 'vector 'read-from-string (ppcre:split #\, line)))))))




(defun cleanData (data lista)
	(cond ((> (length data)0) (cons (append (list(mapcar '/ (quitarultimo (cdr(array-to-list (car data))))'(100 100 100 100 100)))(list(list(nth 5 (cdr(array-to-list (car data)))))))(cleanData (cdr data) lista)))
			(t lista)
	)
)


(defun array-to-list (array)
  (let* ((dimensions (array-dimensions array))
         (depth      (1- (length dimensions)))
         (indices    (make-list (1+ depth) :initial-element 0)))
    (labels ((recurse (n)
               (loop for j below (nth n dimensions)
                     do (setf (nth n indices) j)
                     collect (if (= n depth)
                                 (apply #'aref array indices)
                               (recurse (1+ n))))))
      (recurse 0))))
# Trabajo-de-investigacion
Repositorio para el paradigma funcional realizado en Racket
#lang racket
;Se requiere ver la documentación de Racket en https://docs.racket-lang.org/csv-reading/index.html
;Se importa un paquete de racket para el manejo de archivos csv
(require csv-reading)

(define filename "notasAlumnos.csv");se define el archivo csv

;Se define lee una linea del archivo csv y se convierte en una lista de strings
(define next-row  
  (make-csv-reader
   (open-input-file filename)
   '((separator-chars            #\,)
     (strip-leading-whitespace? . #t)
     (strip-trailing-whitespace? . #t))))

(define(siguiente l )
  (cdr l))

;se define una funcion para almacenar cada linea y almacenarla en una lista
(define dataAlumnos (csv->list next-row))

;metodo para cambiar de tipo el 3er elemento de la lista
(define (cambiar3eDeTipo l)
  (list-update  l 2 string->number ))
  

;Metodo insertar un elemento al inicio 
(define(insertarAlInicio dato l )
     (list* dato l))

;Se convierte de lista a Lista Enlazada
(define (listaToListaE  data lista_enlazada puntero)
  (if(empty? (siguiente data))
     (insertarAlInicio (cambiar3eDeTipo (car data)) lista_enlazada)
     (insertarAlInicio (cambiar3eDeTipo (car data)) (listaToListaE  (cdr data) lista_enlazada (add1 puntero)))
     ))

;Se almacena la data en la variable listaEnlazadaAlumnos
(define listaEnlazadaAlumnos (list* (listaToListaE dataAlumnos  '() 0)))

;Problema A

(define (promedio codigoAlumno listaE acumulador contador puntero) 
  (if(empty? listaE)
     (/ acumulador contador)
     (if (equal? codigoAlumno (first (car listaE)))
         (promedio codigoAlumno (cdr listaE) (+ acumulador (third  (car listaE))) (add1 contador) (add1 puntero))
         (promedio codigoAlumno (cdr listaE) acumulador contador (add1 puntero))
     )
   )
 )

(define (promedioC stdCode); codigo del alumno
  (promedio stdCode listaEnlazadaAlumnos 0 0 0))

(define PromedioCursos (promedioC "20218270")) ;El codigo del alumno es un string
  
;Problema B
(define (Contar codigoCurso listaE contador puntero) 
  (if(empty? listaE)
     contador
     (if (equal? codigoCurso (second (car listaE)))
         (Contar codigoCurso (cdr listaE) (add1 contador) (add1 puntero))
         (Contar codigoCurso (cdr listaE) contador (add1 puntero))
     )
   )
 )
(define (ContarA courseCode) ;codigo del curso
  (Contar courseCode listaEnlazadaAlumnos 0 0))

(define ContarAlumnos (ContarA "92276")) ;El codigo del curso es un string

;Problema C
(define (verificar codigoAlumno codigoCurso nota listaE puntero listaELlena)
  (if(empty? listaE) 
     (insertarAlInicio (list codigoAlumno codigoCurso nota) listaELlena);se registro correctamente
     (if (and (equal? codigoAlumno (first (car listaE))) (equal? codigoCurso (second (car listaE))))
         "ERROR: El alumno ya está registrado en el curso"
         (verificar codigoAlumno codigoCurso nota (cdr listaE) (add1 puntero) listaELlena)
     )
   )
  )
(define (verificarA codigoAlumno codigoCurso nota)
  (verificar codigoAlumno codigoCurso nota listaEnlazadaAlumnos 0 listaEnlazadaAlumnos))

(define verificarAlumnos1 (verificarA "20147836" "10047" 10)) ;Ejemplo 1 (error)

(define verificarAlumnos2 (verificarA "20218270" "10047" 16)) ;Ejemplo 2 (agregar)

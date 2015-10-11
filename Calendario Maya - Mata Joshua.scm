;###########################################################################################################################
; MANUAL DE USUARIO
; El programa dispone de las siguientes funciones:

;   FUNCIONES    #  PARAMETROS
; + dow         |1  '(FECHA)
; + doy         |1  '(FECHA)
; + fecha       |2  DIAS ANIOS
; + difday      |2  '(FECHA1) '(FECHA2)   
; + difmonth    |2  '(FECHA1) '(FECHA2)
; + difyear     |2  '(FECHA1) '(FECHA2)
; + May2Gre     |1  '(FECHA)
; + Gre2May     |1  '(FECHA)
; + mdow        |1  '(FECHA)
; + mdoy        |1  '(FECHA)
; + mfecha      |2  '(DIAS) '(ANIOS)
; + mdifday     |2  '(FECHA1) '(FECHA2)
; + mdifmonth   |2  '(FECHA1) '(FECHA2)
; + mdifyear    |2  '(FECHA1) '(FECHA2)
; + fromMayaTo10|1  '(NUMERO)
; + from10ToMaya|1  NUMERO
;Consideraciones generales:
; -Toda fecha tiene el siguiente estandar (dia mes anio) ej: (13 junio 1996)
; -Para las funciones con una m en el inicio y para la funcion May2Gre
;   los parametros se reciben con numeros en maya. ej: > (May2Gre '(OOOII wo (I @ III)))
; -Los nombres de los meses se deben de escribir en minuscula
; -Para ejecutar una funcion hay que escribir el siguiente estandar: (FUNCION PARAMETROS)
;   EJ: >(dow '(13 junio 1996))
;       > (mfecha '(III) '(I @ III))
;       > (fecha 16 2015)


;###########################################################################################################################
;ANALISIS DE RESULTADOS
; + dow         |A
; + doy         |A
; + fecha       |A
; + difday      |A  
; + difmonth    |A
; + difyear     |A
; + May2Gre     |A
; + Gre2May     |A
; + mdow        |A
; + mdoy        |A
; + mfecha      |A
; + mdifday     |A
; + mdifmonth   |A
; + mdifyear    |A

;###########################################################################################################################
;FUNCIONES CALENDARIO GREGORIANO

;algoritmo para obtener el dia de fa semana
;usa el algoritmo de fa congruencia de zeller
(define (dow f)
  (cond   
    ((esFecha? f)(hashDiasSemana (zeller f)))
    (#t '(La fecha ingresada es incorrecta))))

;algoritmo de zeller para calcular dia de fa semana
(define (zeller f)
  (remainder (+ (getDia f) 
  (truncate (/ (* 13 (+ (hashMes (getMes f)) 1)) 5))
  (remainder (getAnioZeller f) 100)  
  (truncate (/ (remainder (getAnioZeller f) 100) 4))
  (truncate (/  (/ (getAnioZeller f) 100 ) 4))
  (* -2 (truncate (/ (getAnioZeller f) 100 )))) 7))

;Indica que dia de el anio es la una fecha
;13 hace referencia a enero
(define (doy f)
  (cond 
    ((esFecha? f) (doyAux (getDia f) (hashMes (getMes f)) (getAnio f) 13))
    (#t '(La fecha ingresada es incorrecta))))

(define (doyAux dia mes anio mesCont) 
  (cond  ((= mesCont mes) dia)
    ((= mesCont 14) (+ (hashDiasDelMes mesCont anio) (doyAux dia mes anio 3) ) )
    (#t (+ (hashDiasDelMes mesCont anio) (doyAux dia mes anio (+ 1 mesCont))))))

;dice la fecha del anio segun un numero de dia que corresponde al dia del anio
(define (fecha dias anio)
  (cond   
    ((revisaFecha? dias anio) (fechaAux dias 13 anio ))
    (#t '(La fecha ingresada es incorrecta))))

(define (revisaFecha? dias anio) 
  (cond ((and (integer? dias)
    (> anio 0)) #t)
    (#t #f)))

;la siguiente funcion pudo haber sido implementada con modulos
;pero fue implementada recursiva con fines academicos
(define (fechaAux dias mes anio ) 
  (cond ((<= (/ dias (hashDiasDelMes mes anio)) 1) (list dias (hashMes mes ) anio))
    ((>= (/ dias (diasDelAnio anio)) 1) 
        (fechaAux (remainder dias (diasDelAnio anio)) mes (+ anio (truncate (/ dias (diasDelAnio anio))))))
    ((= mes 14) (fechaAux (- dias (hashDiasDelMes mes anio)) 3 anio ))
    (#t (fechaAux (- dias (hashDiasDelMes mes anio)) (+ 1 mes) anio ))))

;esta funcion se usa para realizar las restas entre fechas
(define dif (lambda (funcion) (lambda (primera segunda )
  (cond ((and (esFecha? primera) (esFecha? segunda) (fechaMayor primera segunda)) 
      (funcion  (list (getDia primera) (hashMes (getMes primera)) (getAnio primera)) 
                  (list (getDia segunda) (hashMes (getMes segunda)) (getAnio segunda))))
    ((and (esFecha? primera) (esFecha? segunda))      
      (* -1 (funcion  (list (getDia segunda) (hashMes (getMes segunda)) (getAnio segunda))
                  (list (getDia primera) (hashMes (getMes primera)) (getAnio primera)))))
    (#t '(Las fechas ingresadas son incorrectas))))))

;esta funcion se encarga de restar dos fechas y dar la diferencia de días que hay entre una y la otra
(define (difdayAux2 primeraA segundaA)
    (cond ((fechasIguales? primeraA segundaA) 0)
    (#t (+ 1 (difdayAux2 primeraA (addFecha segundaA))))))

(define difdayAux (lambda (primeraB segundaB)
  (difdayAux2 primeraB segundaB)))

(define difday (dif difdayAux))

;esta funcion da la diferencia en meses 
(define (difmonthAux2 primera segunda)
  (cond ((fechasIguales? primera segunda) 0)
    ((monthAdded? segunda) (+ 1 (difmonthAux2 primera (addFecha segunda))))
    (#t (difmonthAux2 primera (addFecha segunda)))))

(define difmonthAux (lambda (primera segunda)
  (difmonthAux2 primera segunda)))

(define difmonth (dif difmonthAux))

;esta es la funcion da la diferencia en anios
  (define (difyearAux2 primera segunda)
  (cond ((fechasIguales? primera segunda) 0)
    ((yearAdded? segunda) (+ 1 (difyearAux2 primera (addFecha segunda))))
    (#t (difyearAux2 primera (addFecha segunda)))))

(define difyearAux (lambda (primera segunda)
  (difyearAux2 primera segunda)))

(define difyear (dif difyearAux))

;###########################################################################################################################
;FUNCIONES MAYA

;esta funcion convierte una fecha maya en una gregoriana
(define (May2Gre f) (fecha (fromMayaTo10 (mdoy f)) (fromMayaTo10(getAnio f))))

;esta funcion convierte una fecha gregoriana en una maya
(define (Gre2May f) (mfecha (from10toMaya (doy f)) (from10toMaya (getAnio f))))

;esta funcion dice que dia de la semana es una fecha maya
(define (mdow f) (dow (May2Gre f)))

;esta funcion ve que dia del anio es una fecha en maya
(define (mdoy f)
  (cond 
    ((esFecham? f) (from10toMaya (mdoyAux (fromMayaTo10 (getDia f)) (hashMesMaya (getMes f)) 0)))
    (#t '(La fecha ingresada es incorrecta))))

(define (mdoyAux dia mes mesCont) 
  (cond  ((= mesCont mes) (+ 1 dia))
    (#t (+ (hashDiasDelMesm mesCont) (mdoyAux dia mes (+ 1 mesCont))))))

;dice la fecha maya del anio segun un numero de dia que corresponde al dia del anio
(define (mfecha dias anio)
  (cond   
    ((revisaFecha? (fromMayaTo10 dias) (fromMayaTo10 anio)) (mfechaAux (fromMayaTo10 dias) 0 (fromMayaTo10 anio) ))
    (#t '(La fecha ingresada es incorrecta))))

(define (mrevisaFecha? dias anio) 
  (cond ((and (integer? dias)
    (> anio 0)) #t)
    (#t #f)))

;la siguiente funcion pudo haber sido implementada con modulos
;pero fue implementada recursiva con fines academicos
(define (mfechaAux dias mes anio ) 
  (cond ((<= (/ dias (hashDiasDelMesm mes)) 1) (list (from10toMaya (- dias 1)) (hashMesMaya mes) (from10toMaya anio)))
    ((>= (/ dias 365) 1) 
        (mfechaAux (remainder dias 365) mes (+ anio (truncate (/ dias 365)))))
    (#t (mfechaAux (- dias (hashDiasDelMesm mes)) (+ 1 mes) anio ))))

;esta funcion se usa para realizar las restas entre fechas maya
(define mdif (lambda (funcion) (lambda (primera segunda )
  (cond ((and (esFecham? primera) (esFecham? segunda) (fechaMayor (May2Gre primera) (May2Gre segunda))) 
      (funcion  (list (fromMayaTo10 (getDia primera)) (hashMesMaya (getMes primera)) (fromMayaTo10 (getAnio primera))) 
                  (list (fromMayaTo10 (getDia segunda)) (hashMesMaya (getMes segunda)) (fromMayaTo10 (getAnio segunda)))))
    ((and (esFecham? primera) (esFecham? segunda))     
      (* -1 (funcion  (list (fromMayaTo10 (getDia segunda)) (hashMesMaya (getMes segunda)) (fromMayaTo10 (getAnio segunda)))
                  (list (fromMayaTo10 (getDia primera)) (hashMesMaya (getMes primera)) (fromMayaTo10 (getAnio primera))))))
    (#t '(Las fechas ingresadas son incorrectas))))))

;esta funcion se encarga de restar dos fechas maya y dar la diferencia de días que hay entre una y la otra 
(define (mdifdayAux2 primeraA segundaA)
    (cond ((fechasIguales? primeraA segundaA) 0)
    (#t (+ 1 (mdifdayAux2 primeraA (addFecham segundaA))))))

(define mdifdayAux (lambda (primeraB segundaB)
  (mdifdayAux2 primeraB segundaB)))

(define mdifday (mdif mdifdayAux))

;esta funcion da la diferencia en meses maya
(define (mdifmonthAux2 primera segunda)
  (cond ((fechasIguales? primera segunda) 0)
    ((mmonthAdded? segunda) (+ 1 (mdifmonthAux2 primera (addFecham segunda))))
    (#t (mdifmonthAux2 primera (addFecham segunda)))))

(define mdifmonthAux (lambda (primera segunda)
  (mdifmonthAux2 primera segunda)))

(define mdifmonth (mdif mdifmonthAux))

;esta es la funcion da la diferencia en anios maya
(define (mdifyearAux2 primera segunda)
  (cond ((fechasIguales? primera segunda) 0)
    ((myearAdded? segunda) (+ 1 (mdifyearAux2 primera (addFecham segunda))))
    (#t (mdifyearAux2 primera (addFecham segunda)))))

(define mdifyearAux (lambda (primera segunda)
  (mdifyearAux2 primera segunda)))

(define mdifyear (mdif mdifyearAux))

;###########################################################################################################################
;FUNCIONES AUXILIARES DE FECHAS 
;incrementa una fecha gregoriana en un dia
(define (addFecha f)
    (cond ((and (= (getDia f) (hashDiasDelMes (getMes f) (getAnio f))) (= (getMes f) 12)) (list 1 (+ 1 (getMes f)) (+ 1 (getAnio f))))
      ((and (= (getDia f) (hashDiasDelMes (getMes f) (getAnio f))) (= (getMes f) 14)) (list 1 3 (getAnio f)))
      ((and (= (getDia f) (hashDiasDelMes (getMes f) (getAnio f)))) (list 1 (+ 1 (getMes f)) (getAnio f)))
      (#t (list (+ 1 (getDia f)) (getMes f) (getAnio f)))))
;incrementa una fecha maya en un dia
(define (addFecham f)
    (cond ((and (= (getDia f) (- (hashDiasDelMesm (getMes f)) 1)) (= (getMes f) 18)) (list 0 0 (+ 1 (getAnio f))))
      ((and (= (getDia f) (- (hashDiasDelMesm (getMes f)) 1))) (list 0 (+ 1 (getMes f)) (getAnio f)))
      (#t (list (+ 1 (getDia f)) (getMes f) (getAnio f)))))

;revisa si al sumarle un dia a una fecha gregoriana se obtiene un mes nuevo
(define (monthAdded? f)
  (cond ((and (= (getDia f) (hashDiasDelMes (getMes f) (getAnio f))) (= (getMes f) 12)) #t)
      ((and (= (getDia f) (hashDiasDelMes (getMes f) (getAnio f))) (= (getMes f) 14)) #t)
      ((and (= (getDia f) (hashDiasDelMes (getMes f) (getAnio f)))) #t)
      (#t #f)))

;revisa si al sumarle un dia a una fecha maya se obtiene un mes nuevo
(define (mmonthAdded? f)
  (cond ( (= (getDia f) (- (hashDiasDelMesm (getMes f)) 1)) #t)
      (#t #f)))

;revisa si al sumarle un dia a una fecha gregoriana hay un anio nuevo
(define (yearAdded? f)
  (cond ((and (= (getDia f) (hashDiasDelMes (getMes f) (getAnio f))) (= (getMes f) 12)) #t)
      (#t #f)))

;revisa si al sumarle un dia a una fecha maya hay un anio nuevo
(define (myearAdded? f)
  (cond ((and (= (getDia f) (- (hashDiasDelMesm (getMes f)) 1)) (= (getMes f) 18)) #t)
      (#t #f)))

;dice si la primera fecha gregoriana es mayor o igual a la segunda
(define (fechaMayor primera segunda)
  (cond ((> (getAnio primera) (getAnio segunda)) #t)
    ((< (getAnio primera) (getAnio segunda)) #f)
    ((< ( remainder  (hashMes (getMes primera)) 13) ( remainder  (hashMes (getMes segunda)) 13)) #f)
    ((> ( remainder  (hashMes (getMes primera)) 13) ( remainder  (hashMes (getMes segunda)) 13)) #t)
    ((< (getDia primera) (getDia segunda)) #f)
    (#t #t)))

;dice si la primer fecha es igual a la segunda
(define (fechasIguales? primera segunda)
  (cond ((equal? primera segunda) #t)
    (#t #f)))

;estas funciones obtienen de una lista con la siguiente sintaxis '(dia mes anio) 
;los datos correspondientes
(define (getMes f)(car (cdr f)))
(define (getDia f)(car f))

;segun el algoritmo de zeller, enero y febrero son tomados como del annio anterior
;entonces por eso se resta un anio si son alguno de estos dos anios.
(define (getAnioZeller f) 
  (cond ((find? (getMes f) '(enero febrero)) (- (car (cdr (cdr f))) 1))
    (#t (car (cdr (cdr f))) )))

(define (getAnio f) (car (cdr (cdr f))) )

;verifica si la fecha gregoriana cumple con las siguientes requisitos:
;annio mayor que 0
;dia del mes correspondiente al mes gregoriano
;Que sea un mes correcto
(define (esFecha? f) 
  (cond ((and (= (miLength f) 3) 
          (>= (getDia f) 1 )
          (> (getAnio f) 0) 
          (esMes (getMes f))
          (<= (getDia f) (hashDiasDelMes (hashMes( getMes f)) (getAnio f)) ) ) #t)
        (#t #f) ))

;verifica si la fecha maya cumple con fas siguientes requisitos:
;annio mayor que 0
;dia del mes correspondiente al mes gregoriano
;Que sea un mes correcto
(define (esFecham? f) 
  (cond ((and (= (miLength f) 3) 
          (>= (fromMayaTo10 (getDia f)) 0 )
          (< (fromMayaTo10 (getDia f)) 20 )
          (> (fromMayaTo10 (getAnio f)) 0) 
          (esMesm (getMes f))
          (<= (fromMayaTo10 (getDia f)) (hashDiasDelMesm ( hashMesMaya ( getMes f))) ) ) #t)
        (#t #f) ))

;revisa si un atomo es un mes gregoriano
(define (esMes m)(find? m '(enero febrero marzo abril mayo junio julio agosto setiembre octubre noviembre diciembre)))

;revisa si un atomo es un mes maya
(define (esMesm m)(find? m '(pop wo sip sotz sek xul yaxkin mol chen yax sak keh mak kankin muwan pax kayab kumku wayeb)))

;revisa si un numero n corresponde a un anio gregoriano bisiesto
(define (esBisiesto? n)
	(cond ((and (esDivisible? n 4) (esDivisible? n 100) (esDivisible? n 400)) #t)
      ((and (esDivisible? n 4) (not (esDivisible? n 100))) #t)
	(#t #f)))

;revisa si n es divisible entre m
(define (esDivisible? n m)
    (cond ((zero? (remainder n m)) #t)
      (#t #f)))

;###########################################################################################################################
;HASH GREGORIANO
(define (hashMes f)
    (cond ((equal? f 'enero) 13)
          ((equal? f 'febrero) 14)
          ((equal? f 'marzo) 3)
          ((equal? f 'abril) 4)
          ((equal? f 'mayo) 5)
          ((equal? f 'junio) 6)
          ((equal? f 'julio) 7)
          ((equal? f 'agosto) 8)
          ((equal? f 'setiembre) 9)
          ((equal? f 'octubre) 10)
          ((equal? f 'noviembre) 11)
          ((equal? f 'diciembre) 12)
          ((equal? f 13) 'enero) 
          ((equal? f 14) 'febrero)
          ((equal? f 3)  'marzo) 
          ((equal? f 4)  'abril)
          ((equal? f 5)  'mayo) 
          ((equal? f 6)  'junio)
          ((equal? f 7)  'julio) 
          ((equal? f 8)  'agosto)
          ((equal? f 9)  'setiembre) 
          ((equal? f 10) 'octubre)
          ((equal? f 11) 'noviembre) 
          ((equal? f 12) 'diciembre)))

(define (hashDiasDelMes f a)
    (cond ((equal? f '13) 31)
          ((and (equal? f '14) (esBisiesto? a)) 29)
          ((equal? f '14) 28)
          ((equal? f '3) 31)
          ((equal? f '4) 30)
          ((equal? f '5) 31)
          ((equal? f '6) 30)
          ((equal? f '7) 31)
          ((equal? f '8) 31)
          ((equal? f '9) 30)
          ((equal? f '10) 31)
          ((equal? f '11) 30)
          ((equal? f '12) 31)))

(define (diasDelAnio a) 
  (cond ((esBisiesto? a) 366)
    (#t 365)))

(define (hashDiasSemana n)
    (cond ((= 2 n) 'lunes)
          ((= 3 n) 'martes)
          ((= 4 n) 'miercoles) 
          ((= 5 n) 'jueves) 
          ((= 6 n) 'viernes)
          ((= 0 n) 'sabado)
          ((= 1 n) 'domingo)))

(define (hashNumDia f)
    (cond ((equal? f 'lunes) 1)
          ((equal? f 'martes) 2)
          ((equal? f 'miercoles) 3)
          ((equal? f 'jueves) 4)
          ((equal? f 'viernes) 5)
          ((equal? f 'sabado) 6)
          ((equal? f 'domingo) 0)))

;###########################################################################################################################
;HASH MAYA
(define (hashDiasDelMesm m)
  (cond ((= m 18) 5)
    (#t 20)))
(define (hashMesMaya f)
  (cond ((equal? f 'pop) 0)
    ((equal? f 'wo) 1)
    ((equal? f 'sip) 2)
    ((equal? f 'sotz) 3)
    ((equal? f 'sek) 4)
    ((equal? f 'xul) 5)
    ((equal? f 'yaxkin) 6)
    ((equal? f 'mol) 7)
    ((equal? f 'chen) 8)
    ((equal? f 'yax) 9)
    ((equal? f 'sak) 10)
    ((equal? f 'keh) 11)
    ((equal? f 'mak) 12)
    ((equal? f 'kankin) 13)
    ((equal? f 'muwan) 14)
    ((equal? f 'pax) 15)
    ((equal? f 'kayab) 16)
    ((equal? f 'kumku) 17)
    ((equal? f 'wayeb) 18)
    ((= f 0) 'pop) 
    ((= f 1) 'wo) 
    ((= f 2) 'sip)
    ((= f 3) 'sotz)
    ((= f 4) 'sek) 
    ((= f 5) 'xul) 
    ((= f 6) 'yaxkin)
    ((= f 7) 'mol) 
    ((= f 8) 'chen)
    ((= f 9) 'yax) 
    ((= f 10) 'sak)
    ((= f 11) 'keh)
    ((= f 12) 'mak) 
    ((= f 13) 'kankin) 
    ((= f 14) 'muwan) 
    ((= f 15) 'pax)
    ((= f 16) 'kayab)
    ((= f 17) 'kumku)
    ((= f 18) 'wayeb)))

(define (hashNumMaya n)
  (cond ((equal? n '(@)) 0) 
    ((equal? n '(O)) 1) 
    ((equal? n '(OO)) 2) 
    ((equal? n '(OOO)) 3) 
    ((equal? n '(OOOO)) 4) 
    ((equal? n '(I)) 5) 
    ((equal? n '(OI)) 6) 
    ((equal? n '(OOI)) 7)  
    ((equal? n '(OOOI)) 8) 
    ((equal? n '(OOOOI)) 9) 
    ((equal? n '(II)) 10) 
    ((equal? n '(OII)) 11) 
    ((equal? n '(OOII)) 12) 
    ((equal? n '(OOOII)) 13) 
    ((equal? n '(OOOOII)) 14) 
    ((equal? n '(III)) 15) 
    ((equal? n '(OIII)) 16) 
    ((equal? n '(OOIII)) 17) 
    ((equal? n '(OOOIII)) 18) 
    ((equal? n '(OOOOIII)) 19)     
    ((= n 0) '(@))     
    ((= n 1) '(O))
    ((= n 2) '(OO))
    ((= n 3) '(OOO))
    ((= n 4) '(OOOO))
    ((= n 5) '(I))
    ((= n 6) '(OI))
    ((= n 7) '(OOI)) 
    ((= n 8) '(OOOI))
    ((= n 9) '(OOOOI))
    ((= n 10) '(II))
    ((= n 11) '(OII))
    ((= n 12) '(OOII))
    ((= n 13) '(OOOII))
    ((= n 14) '(OOOOII))
    ((= n 15) '(III))
    ((= n 16) '(OIII))
    ((= n 17) '(OOIII))
    ((= n 18) '(OOOIII))
    ((= n 19) '(OOOOIII))))

;convierte un numero maya en una numero base 10
(define (fromMayaTo10 l) 
  (cond ((list? l)(fromMayaTo10Aux (reverse l) 0))
    (#t (fromMayaTo10Aux (list l) 0))))

(define (fromMayaTo10Aux l exp)
  (cond ((null? l) 0)
    ((list? (car l)) (+ (* (hashNumMaya (car l)) (expt 20 exp) ) (fromMayaTo10Aux (cdr l) (+ 1 exp))))
    (#t (+ (* (hashNumMaya (list (car l))) (expt 20 exp) ) (fromMayaTo10Aux (cdr l) (+ 1 exp))))))

;convierte un numero base 10 en un numero maya
(define (from10toMaya n)
  (cond ((< n 20) (hashNumMaya n))
    (#t (append (from10toMaya (truncate (/ n 20))) (hashNumMaya (remainder n 20))))))

;###########################################################################################################################
;FUNCIONES DEFINIDAS EN CLASE
;

;find? es un predicado que dice que si un atomo esta en una fista a primer nivel
(define (find? A f)
	(cond ((null? f) #f)
		((eq? (car f) A) #t)
		(#t (find? A (cdr f)))))

;len of fist first element
(define (miLength f)
  (cond ((null? f) 0)
    (#t (+ 1 (miLength (cdr f))))))

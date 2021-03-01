;Andres Felipe Arrechea Saa - 1780023
;Nicolas Jaramillo Mayor - 1840558
;Luis felipe valencia - 1824494

#lang eopl



;1 sublistas : list -> int
; Proposito : recibe una lista y retorna el nuemro de sublistas dentro de la misma

(define sublistas (lambda (lst)
                    (if (null? lst )
                        0
                        (if (list? (car lst))
                            (+ 1 (sublistas (car lst)) (sublistas (cdr lst)))
                            (sublistas (cdr lst))
                            )
                        )
                    )
  )


;2 filtro
;filtro : pred list -> list
;proposito: recibe un predicado y una lista y retorna una lista con los elementos de la lista que cumplan
;;con el predicado pred
 
(define filtro
  (lambda (pred lst)
    (if (null? lst)
        empty
        (if (pred (car lst)) 
            (cons (car lst) (filtro pred (cdr lst)))
            (filtro pred (cdr lst))
        )
     )
   )
)


;3 inversion-listas---------------------------------------------------------------------------------------

;aux
;invertir-par: list -> list
;proposito: recibe una lista con dos elementos e invierte su posicion en la lista

(define invertir-par (lambda (lst-par)

                       (if (and (list? lst-par) (= (length lst-par) 2 ) )
                           (list (cadr lst-par)  (car lst-par))
                           "error la lista debe tener longitud 2"
                           )
                       )
  )

;inversion-listas: list -> list
;proposito: recibe una lista cuyos elemelemtos son a su vez listas de dos elementos e invierte la posicion
;de lso elementos dentro de cada sublista interior haciendo uso de la funcion invertir-par

(define inversion-listas (lambda (lst)
                           (if (null? lst)
                               empty
                               (cons (invertir-par (car lst)) (inversion-listas (cdr lst)))
                            )
                           )
  )


;4 situar-en-lista: list int elem -> list
;proposito: recibe una lista, una posicion (int) y un elemento y devuelve la lista con el elemento elem en la posicion
;pos (elimina el elemento que s encontraba anteriormente en dica posicion)
(define situar-en-lista (lambda (lst pos elem)
                          (if (= pos 0)
                              (cons elem (cdr lst))
                              (cons (car lst) (situar-en-lista (cdr lst) (- pos 1) elem))
                              )
                          )
  )

;5 ordenar------------------------------------------------------------------------------------------------------------

;aux
;mayor: list int -> int
;proposito: encontrar el mayor en una lista estableciendo un candidato
(define mayor (lambda (lst candidato)
                (if (null? lst)
                    candidato
                    (if (> (car lst) candidato)
                        (mayor (cdr lst) (car lst))
                        (mayor (cdr lst) candidato)
                        )
                    )
                )
  )

;aux
;mayo-listar: list-> int
;proposito: encontrar el mayor en una lista haciendo uso de la funcion mayor
;y estableciendo como candidato el primero de la lista

(define mayor-lista (lambda (lst) 
                      (mayor lst (car lst))
                      )
  )

;aux
;quitar elemento : list elem -> list
;proposito: quitar un elemento de una lista
(define quitar-elemento (lambda (lst elem)
                          (if (null? lst)
                               '()
                               (if (= (car lst) elem)
                                   (cdr lst)
                                   (cons (car lst) (quitar-elemento (cdr lst) elem))
                                   )
                               )
                          )
  )
 


;aux
;ordenar-mayor: list -> list
;proposito: ordenar una lista de menor a mayor, tomando el mayor de la lista, el mayor del resto de la lista,
;el mayor del resto del resto de la lista, etc
(define ordenar-mayor (lambda (lst)
                        (if (null? lst)
                            empty
                            (cons (mayor-lista lst) (ordenar-mayor (quitar-elemento lst (mayor-lista lst))))
                         )
                        )
  )

;aux
;ordenar-mayor: list -> list
;proposito: invertir el orden de una lista
(define invertir-lista (lambda (lst)
                         (if (null? lst)
                             empty
                              (append (invertir-lista (cdr lst)) (list (car lst)))
                              )))

;ordenar: simb list -> list
;proposito: ordenar una lista de menor a mayor, o de mayor a menor segun el simbolo que se le pase
;sienso < para ordenar de menor a mayor y > para ordenar de mayor a menor
(define ordenar (lambda (simb lst)
                  (if (equal? simb >)
                      (ordenar-mayor lst)
                      (if (equal? simb <)
                          (invertir-lista (ordenar-mayor lst))
                          'error))
                  )
  )


;6
;indice-lista: pred list -> int
;proposito: retorna el indice de el primer elemento de la lista que cumpla con el predicado pred
(define indice-lista (lambda (pred lst)
                     
                       (cond

                         [(null? lst) 0]
                         [(pred (car lst)) 0]
                         [else (+ 1 (indice-lista pred (cdr lst)))]
                         )
                       )
  )

; 7
; contar-ocurrencias : symbol list -> int
; Proposito : Procedimiento que cuenta las ocurrencias de un elemento en un lista  
(define contar-ocurrencias
  (lambda (elem S-list)
    (cond
      [(null? S-list) 0]
      [(list? (car S-list))
       (if (equal? elem (car S-list))
           (+ 1 (contar-ocurrencias elem (cdr(car S-list))) (contar-ocurrencias elem (cdr S-list)))
           (+(contar-ocurrencias elem (car S-list)) (contar-ocurrencias elem (cdr S-list)))
        )
       ]
      [(if(equal? elem (car S-list))
          (+ 1 (contar-ocurrencias elem (cdr S-list)))
          (contar-ocurrencias elem (cdr S-list))
          )
       ]
     )
   )
)


; 8
; intercambio : elem elem list -> list
; Proposito : recibe dos elementos elem1 y elem2 y una lista y retorna una nueva lista con los elementos intercambiados
; elem1 por elem2 y elem2 por elem1

  (define intercambio (lambda (elem1 elem2 lst)

                        (cond
                          [(null? lst) empty]
                          [(equal? (car lst) elem1) (cons elem2 (intercambio elem1 elem2 (cdr lst)))]     
                          [(equal? (car lst) elem2) (cons elem1 (intercambio elem1 elem2 (cdr lst)))]
                          [(list? (car lst)) (cons (intercambio elem1 elem2 (car lst)) (intercambio elem1 elem2 (cdr lst))) ]
                          [else (cons (car lst) (intercambio elem1 elem2 (cdr lst)))]
                          )
                        )
    )


;9 producto------------------------------------------------------------------------------------------------------------------

; aux
; producto-aux : list list -> list
; Proposito : retorna una lista cuyos elementos son pares cuyo primer elemento es el primer elemento de la lista 1
; pero el segundo elemento del par es un elemento de la lista 2 (asi hasta qeu cada elemento de la lista 2 este emparejado
; con el primer elemento de la lista 1)

( define producto-aux (lambda (lst1 lst2)
                   (if (null? lst2)
                       empty
                       (cons (append (list (car lst1)) (list (car lst2)) ) (producto-aux lst1 (cdr lst2)))
                       )
                   )
  )


; producto: list list -> list
; Proposito : retorna una lista de pares que representan todas las posibles combinaciones de 2 elementos
; cuyo primer elemento pertenese a la lista 1 y el segundo elemento a la lista 2

(define producto (lambda (lst1 lst2)
                   (if (null? lst1)
                       empty
                       (append (producto-aux lst1 lst2) (producto (cdr lst1) lst2)))
                   )
  )


;10 filter-acum
(define filter-acum (lambda (a b F acum filter)


                      (cond
                        [(and (> acum b) (or (equal? F *) (equal? F /))) 1 ]
                        [(and (> acum b) (not (or (equal? F *) (equal? F /)))) 0 ]
                        [(and (and (>= acum a) (<= acum b)) (filter acum))
                         (F acum  (filter-acum a b F (+ acum 1) filter)) ]
                        
                        [else  (filter-acum a b F (+ 1 acum) filter) ]
                        )
                      )
  )

; 11
; list-append : list list -> list
; Proposito : fuciona los elementos de dos listas en una sola cuyos elementos son
; los elementos de la lista 1 seguidos de los elementos de la lista 2

 (define list-append (lambda (lst1 lst2)
                       (cond
                         [(null? lst1) lst2]
                         [(null? lst2) lst1] 
                         [else (cons  (car lst1 ) (list-append (cdr lst1) lst2))]

                         ) 
                       )
   )

; 12
; Operate : list1 list2 -> int
; Proposito : Procedimiento que retorna el resultado de aplicar sucesivamente las operaciones en list1 a los valores en list2

(define operate
  (lambda (lrator lrands)
    (letrec
        (
         (lst1(invertir-lista lrands))
         (lst2 (invertir-lista lrator) )
     
         (f (lambda(listOpe listnum)
            (if (null? (cdr listOpe))
                ((car listOpe)(cadr listnum)(car listnum))
                ((car listOpe)(f (cdr listOpe)(cdr listnum))(car listnum)))))
         )
         (f lst2 lst1)      
    )
   )
 )


; 13
; zip: procedure list list -> list
; Proposito : retorna una lista cuyos elementos son el resultado de aplicar el procedimiento F pasandole
; como parametros los elementos de posiciones iguales dentro de cada lista, por lo que las lista a trabajar
; deben tener igual longitud
(define zip (lambda (F lst1 lst2)

              (cond
                [(not (= (length lst1) (length lst2))) "error, las listas deben tener igual longitud"]
                [(or (null? lst1) (null? lst2)) empty]
                [else (cons (F (car lst1) (car lst2) ) (zip F (cdr lst1) (cdr lst2)))]
                )
              )
  )

; 14
; path: int list -> list
; Proposito: procedimiento que busca el camino del elemento n dentro del arbol binario de busqueda BST
(define path
  (lambda (n BST)
    (cond
      [(equal? n (car BST)) empty]
      [(> n (car BST))(cons 'right (path n (caddr BST)))]
      [(< n (car BST))(cons 'left (path n (cadr BST)))]
      )
    )
  )

(define lst1 '(14 (7 () (12 () ()))(26 (20 (17 () ())())(31 () ()))))
(define lst2 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
;pruebas
;(path 17 lst1) -> (right left left)
;(path 12 lst1) -> (left right)
;(path 13 lst2) -> (right right left)
;(path 7 lst2) -> (left right right)

;15 compose
(define compose (lambda (proc1 proc2)
                  (lambda (val)
                    (proc1 (proc2 val))
                    
                    )

                  )
  )

;16 carCdr
(define carCdr-resolve (lambda (elem lst)
                         (cond
                           [(null? lst) empty]
                           [(equal? (car lst) elem) 'car]
                           [(list? (car lst)) (if (zero? (contar-ocurrencias elem (car lst)))
                                               (cons 'compose (cons (carCdr-resolve elem (cdr lst)) '(cdr)))
                                               (cons 'compose (cons (carCdr-resolve elem (car lst)) '(car)))
                                               )
                           ]
                           [else (cons 'compose (cons (carCdr-resolve elem (cdr lst)) '(cdr)))]
                           )
                         )
  )
(define carCdr (lambda (elem lst errvalue)
                 (if (zero? (contar-ocurrencias elem lst))
                     errvalue
                     (carCdr-resolve elem lst)
                   )
                 )
  )

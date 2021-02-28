#lang eopl



;1 sublistas
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


;3 inversion listas

(define invertir-par (lambda (lst-par)

                       (if (and (list? lst-par) (= (length lst-par) 2 ) )
                           (list (cadr lst-par)  (car lst-par))
                           "error la lista debe tener longitud 2"
                           )
                       )
  )


(define inversion-listas (lambda (lst)
                           (if (null? lst)
                               empty
                               (cons (invertir-par (car lst)) (inversion-listas (cdr lst)))
                            )
                           )
  )


;4 situar en lista
(define situar-en-lista (lambda (lst pos elem)
                          (if (= pos 0)
                              (cons elem (cdr lst))
                              (cons (car lst) (situar-en-lista (cdr lst) (- pos 1) elem))
                              )
                          )
  )

;5 ordenar---------------------------------------------------------------------------------------------------
;;encontrar el mayor en una lista estableciendo un candidato
(define mayor (lambda (lst test)
                (if (null? lst)
                    test
                    (if (> (car lst) test)
                        (mayor (cdr lst) (car lst))
                        (mayor (cdr lst) test)
                        )
                    )
                )
  )

 
;;encontrar el mayor en una lista estableciendo como candidato el primero de la lista
(define mayor-lista (lambda (lst) 
                      (mayor lst (car lst))
                      )
  )


;;quitar un elemento de una lista
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
 


;;ordena la lista de mayor a menor
(define ordenar-mayor (lambda (lst)
                        (if (null? lst)
                            empty
                            (cons (mayor-lista lst) (ordenar-mayor (quitar-elemento lst (mayor-lista lst))))
                         )
                        )
  )

;;invertir el oorden de una lista
(define invertir-lista (lambda (lst)
                         (if (null? lst)
                             empty
                              (append (invertir-lista (cdr lst)) (list (car lst)))
                              )))

;;ordena de menor a mayor o de mayor a menor segun el simbolo que se le pase
(define ordenar (lambda (simb lst)
                  (if (equal? simb >)
                      (ordenar-mayor lst)
                      (if (equal? simb <)
                          (invertir-lista (ordenar-mayor lst))
                          'error))
                  )
  )
;6 indice lista
(define indice-lista (lambda (pred lst)
                     
                       (cond

                         [(null? lst) 0]
                         [(pred (car lst)) 0]
                         [else (+ 1 (indice-lista pred (cdr lst)))]
                         )
                       )
  )

; punto 7
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

;8 intercambio
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


;9 producto
(define producto-aux (lambda (lst1 lst2)
                   (if (null? lst2)
                       empty
                       (cons (append (list (car lst1)) (list (car lst2)) ) (producto-aux lst1 (cdr lst2)))
                       )
                   )
  )
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
;11 list-append
 (define list-append (lambda (lst1 lst2)
                       (cond
                         [(null? lst1) lst2]
                         [(null? lst2) lst1] 
                         [else (cons  (car lst1 ) (list-append (cdr lst1) lst2))]

                         ) 
                       )
   )

;punto 12
; InvertirLista : list -> list
; Proposito: Procedimiento que invierte el orden de una lista 
(define invertirLista
  (lambda (lst)
    (if (null? lst)
        empty
        (append (invertirLista (cdr lst)) (list (car lst)))
        )
    )          
  )

; Operate : list1 list2 -> int
; Proposito : Procedimiento que retorna el resultado de aplicar sucesivamente las operaciones en list1 a los valores en list2

(define operate
  (lambda (lrator lrands)
    (letrec
        (
         (lst1(invertirLista lrands))
         (lst2 (invertirLista lrator) )
     
         (f (lambda(listOpe listnum)
            (if (null? (cdr listOpe))
                ((car listOpe)(cadr listnum)(car listnum))
                ((car listOpe)(f (cdr listOpe)(cdr listnum))(car listnum)))))
         )
         (f lst2 lst1)      
    )
   )
 )


;13 zip
(define zip (lambda (F lst1 lst2)

              (cond
                [(not (= (length lst1) (length lst2))) "error, las listas deben tener igual longitud"]
                [(or (null? lst1) (null? lst2)) empty]
                [else (cons (F (car lst1) (car lst2) ) (zip F (cdr lst1) (cdr lst2)))]
                )
              )
  )
; pregunta 14
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

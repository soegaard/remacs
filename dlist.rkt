#lang racket
(provide dcons             ; (dcons element prev next)   like cons
         dempty            ; empty dlist
         last-dcons?       ; is next dempty?
         first-dcons?      ; is prev dempty?
         dlist?            ; is input part of a dlist?
         first-dcons       ; find first dcons in the dlist
         last-dcons        ; find last dcons in the dlist
         list->dlist       ; create dlist with elements from list
         right-dlist->list ; create list with elements from the dcons and dconses to the right
         left-dlist->list  ; create list with elements from the dcons and dconses to the left
         dlist->list       ; convert dlist to list
         dappend!          ; destructively append two lists
         connect-dconses!  ; link two dconses together
         dinsert-before!   ; insert element in a newly allocated dcons before input dcons
         dinsert-after!    ; insert element in a newly allocated dcons after input dcons
         dlist             ; create dlist, like list
         in-right-dlist    ; iterate to the right
         in-left-dlist     ; iterate to the left
         in-dlist          ; iterate through all elements
         dlist             ; match pattern
         right-dlist       ; match pattern
         left-dlist        ; match pattern
         dlist-ref         ; like list-ref, negative indices to the left
         dlist-move        ; find dcons relative to input
         )
         
        
         
;;;
;;; DOUBLE LINKED LIST
;;;

; A doubly linked list is either 
;     (dcons a p n) 
;  or ()
; here a is a list element,
;      p is the previous dcons
;      n is the next dcons.

(struct dcons (a p n) #:mutable #:transparent)
(define dempty '())

; dempty? : any -> boolean
;   does xs represent the empty doubly linked list
(define (dempty? xs) 
  (null? xs))

; last-dcons? : any -> boolean
;   is xs the last dcons in a doubly linked list?
(define (last-dcons? xs)
  (and (dcons? xs)
       (dempty? (dcons-n xs))))

; first-dcons? : any -> boolean
;   is xs the first dcons in a doubly linked list?
(define (first-dcons? xs)
  (and (dcons? xs)
       (dempty? (dcons-p xs))))


; dlist? xs -> boolean
;   is xs a part of a dlist?
(define (dlist? xs)
  (or (dempty? xs)
      (and (dcons? xs)
           (dlist-left? xs)
           (dlist-right? xs))))

(define (dlist-left? xs)
  (or (null? xs)
      (and (dcons? xs)
           (dlist-left? (dcons-p xs)))))

(define (dlist-right? xs)
  (or (null? xs)
      (and (dcons? xs)
           (dlist-right? (dcons-n xs)))))

; last-dcons : dcons -> dcons
;   find the last dcons to the right
(define (last-dcons xs)
  ; loop : dcons -> dcons
  (define (loop xs)    
    (define n (dcons-n xs))
    (if (dempty? n) xs (loop n)))
  (if (dcons? xs)
      (loop xs)
      (error 'last-dcons "non-empty doubly linked list (dcons) expected, got ~a" xs)))

; first-dcons : dcons -> dcons
;   find the first dcons to the left
(define (first-dcons xs)
  ; loop : dcons -> dcons
  (define (loop xs)
    (define p (dcons-p xs))
    (if (dempty? p) xs (loop p)))
  (if (dcons? xs)
      (loop xs)
      (error 'first-dcons "non-empty doubly linked list (dcons) expected, got ~a" xs)))

(define (list->dlist xs)
  (define (recur p xs)
    (cond 
      [(null? xs) dempty]
      [else       (define d (dcons (car xs) p #f))
                  (define n (recur d (cdr xs)))
                  (set-dcons-n! d n)
                  d]))
  (cond 
    [(null? xs) dempty]
    [else       (define d (dcons (car xs) dempty #f))
                (define n (recur d (cdr xs)))
                (set-dcons-n! d n)
                d]))

; right-dlist->list : dlist -> list
;  return a list of the elements in the dconses starting with xs and going right
(define (right-dlist->list xs)
  (if (dcons? xs)
      (cons (dcons-a xs) (right-dlist->list (dcons-n xs)))
      '()))

; left-dlist->list : dlist -> list
;  return a list of the elements in the dconses starting with xs and going left
(define (left-dlist->list xs)
  (if (dcons? xs)
      (cons (dcons-a xs) (left-dlist->list (dcons-p xs)))
      '()))

; dlist->list : dlist -> list
;   return a list of all elements in the dlist in which is a part of of
(define (dlist->list xs)
  (if (dempty? xs)
      '()
      (right-dlist->list (first-dcons xs))))


; connect-dconses! : dcons dcons -> void
(define (connect-dconses! d1 d2)
  (set-dcons-n! d1 d2)
  (set-dcons-p! d2 d1))

; dpappend! : dlist dlist -> dlist
;   Return a dlist containing all elements of xs and ys.
;   If non-empty then the last doncs of xs is returned.
(define (dappend! xs ys)
  (cond
    [(dempty? xs) ys]
    [(dempty? ys) xs]
    [else         (define l (last-dcons xs))
                  (define f (first-dcons ys))
                  (connect-dconses! l f)
                  l]))

; dinsert-before! dlist any -> dcons
;   insert the element a before the dcons xs
;   the doncs allocated for a is returned
(define (dinsert-before! xs a)
  (cond
    [(dempty? xs)
     (dcons a dempty dempty)]
    [(first-dcons? xs)
     (define d (dcons a dempty xs))
     (set-dcons-p! xs d)]
    [else
     (define p (dcons-p xs))
     (define d (dcons a p xs))
     (set-dcons-n! p d)
     (set-dcons-p! xs d)
     d]))

; dinsert-after! dlist any -> dcons
;   insert the element a after the dcons xs
;   the doncs allocated for a is returned
(define (dinsert-after! xs a)
  (cond
    [(dempty? xs)
     (dcons a dempty dempty)]
    [(last-dcons? xs)
     (define d (dcons a dempty xs))
     (set-dcons-p! xs d)]
    [else
     (define p (dcons-p xs))
     (define d (dcons a p xs))
     (set-dcons-n! p d)
     (set-dcons-p! xs d)
     d]))

(define (dlist-split-before! xs)
  (cond
    [(first-dcons? xs) (values dempty xs)]
    [else              (define p (dcons-p xs))
                       (set-dcons-n! p dempty)
                       (set-dcons-p! xs dempty)
                       (values p xs)]))

(define (dlist-split-after! xs)
  (cond
    [(last-dcons? xs)  (values xs dempty)]
    [else              (define n (dcons-n xs))
                       (set-dcons-n! xs dempty)
                       (set-dcons-p! n dempty)
                       (values xs n)]))



; dlist-move : dcons integer -> dcons
;   return dcons  n to the right (if n positive)
;   return dcons -n to the left  (if n negative)
(define (dlist-move xs n)
  (cond 
    [(= n 0) xs]
    [(> n 0) (right-dlist-move xs n)]
    [(< n 0) (left-dlist-move xs (- n))]
    [else (error 'dlist-move "expected integer, got ~a" n)]))

(define (right-dlist-move xs n)
  (if (= n 0)
      xs
      (right-dlist-move (dcons-n xs) (- n 1))))

(define (left-dlist-move xs n)
  (if (= n 0)
      xs
      (left-dlist-move (dcons-p xs) (- n 1))))

; dlist-ref : dlist n -> any
;   like list-ref but for dlists
(define (dlist-ref xs n)
  (dcons-a (dlist-move xs n)))

(require (for-syntax syntax/parse))

(define-match-expander right-dlist
  (λ (stx)
    (syntax-parse stx
      [(_)          #'(== dempty)]
      [(_ a)        #'(dcons a _ _)]
      [(_ a . more) #'(dcons a _ (right-dlist . more))])))

(define-match-expander left-dlist
  (λ (stx)
    (syntax-parse stx
      [(_)          #'(== dempty)]
      [(_ a)        #'(dcons a _ _)]
      [(_ a . more) #'(dcons a (dlist . more) _)])))

(define-match-expander dlist
  (λ (stx)
    (syntax-parse stx
      [(_ . more)     #'(app (λ (xs) (dlist->list (first-dcons xs))) (list . more))]))
  (λ (stx)
    (syntax-parse stx
      [(_)       #'dempty]
      [(_ x ...) #'(list->dlist (list x ...))]
      [dl        #'(λ xs (list->dlist xs))])))

(define-sequence-syntax in-right-dlist
  (λ () #'in-right-dlist/proc)
  (λ (stx)
    (syntax-parse stx
      [[(a) (_ xs-expr)]
       #'[(a)
          (:do-in
           ([(xs0) xs-expr])
           (unless (dcons? xs0)
             (raise-type-error 'in-right-dlist "doubly linked list" xs0))
           ([xs xs0])
           (not (dempty? xs))
           ([(ys a) (values (dcons-n xs) (dcons-a xs))])
           #true
           #true
           [ys])]])))

(define-sequence-syntax in-left-dlist
  (λ () #'in-left-dlist/proc)
  (λ (stx)
    (syntax-parse stx
      [[(a) (_ xs-expr)]
       #'[(a)
          (:do-in
           ([(xs0) xs-expr])
           (unless (dcons? xs0)
             (raise-type-error 'in-left-dlist "doubly linked list" xs0))
           ([xs xs0])
           (not (dempty? xs))
           ([(ys a) (values (dcons-p xs) (dcons-a xs))])
           #true
           #true
           [ys])]])))

(define-sequence-syntax in-dlist
  (λ () #'in-dlist/proc)
  (λ (stx)
    (syntax-parse stx
      [[(a) (_ xs-expr)]
       #'[(a) (in-right-dlist (first-dcons xs-expr))]])))

(module+ test (require rackunit)
  (define xs (list->dlist '(1 2 3)))
  (check-true   (dempty? (list->dlist '())))
  (check-equal? (dlist->list xs) '(1 2 3))
  (check-equal? (right-dlist->list xs) '(1 2 3))
  (check-equal? (dcons-a (last-dcons xs)) 3)
  (check-equal? (left-dlist->list (last-dcons xs)) '(3 2 1))
  (check-equal? (right-dlist->list (first-dcons (last-dcons xs))) '(1 2 3))
  (check-equal? (let ()
                  (define xs (list->dlist '(1 2 3)))
                  (define ys (list->dlist '(4 5 6)))
                  (dlist->list (dappend! xs ys)))
                '(1 2 3 4 5 6))
  (check-equal? (map (λ (n) (dlist-ref xs n))              '(0  1 2))  '(1 2 3))
  (check-equal? (map (λ (n) (dlist-ref (last-dcons xs) n)) '(0 -1 -2)) '(3 2 1))
  (check-equal? (let ()
                  (define xs (list->dlist '(1 2 3 4)))
                  (define-values (ys zs) (dlist-split-after! (dlist-move xs 1)))
                  (list (dlist->list ys) (dlist->list zs)))
                '((1 2) (3 4))))


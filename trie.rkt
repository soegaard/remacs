#lang racket
(provide make-empty-completions
         completions-insert-string
         completions-lookup)
;;;
;;; TRIE
;;;

; A quick implementation of tries inspired by page 163-165 in 
; Chris Okasaki's "Purely Functional Data Structures".

; A trie is a finite map which maps keys in form of lists over
; a base type to values. 

; We represent as a triple:

(struct trie (end value map) #:transparent)

; where 
;   end    is a boolean
;   value  is of the base type
;   map    is a finite map from the base type to tries

; A trie containing "ca", "car" and "o" can be drawn as
; a tree, where end and value is drawn at the nodes, and
; the map contains the labels of the subtrees.

;         #f,-
;          /\
;      c  /  \ o
;        /    \
;      #f,-  #t,3
;      /      
;   a /
;    /
;  #t,1
;   |
; r |
;   |
;  #t,2  

; End indicates that a certain key sequence has an associated value.
; In the above trie, the sequence (list #\c) has no associated value,
; where as (list #\c #\a) is associated to a 1.

; Here is one way to represent the finite map:
;(define (fm:empty)      (make-hasheqv))
;(define (fm:empty? m)   (hash-empty? m))
;(define (fm:lookup k m) (hash-ref m k #f))
;(define (fm:bind k v m) (hash-set! m k v) m)
(define (fm:empty)      '())
(define (fm:empty? m)   (null? m))
(define (fm:lookup k m)
  (cond
    [(assoc k m) => cdr]
    [else           #f]))
(define (fm:bind k v m)
  (cons (cons k v) 
        (fm:remove k m)))

(define (fm:remove k m)
  (define found? (for/and ([a (in-list m)]) (equal? (car a) k)))
  (if found?
      (for/list ([a (in-list m)] #:unless (equal? (car a) k)) a)
      m))

; make-empty-empty : -> trie
;   return an empty trie
(define make-empty-trie
  (let ([e (trie #f #f (fm:empty))])
    (lambda () e)))

; empty? : trie -> boolean
;   determine whether the trie is empty
(define (trie-empty? t)
  (and (not (trie-end t))
       (fm:empty? (trie-map t))))

(define (handle-not-found t)
  (if (eq? t #f)
      (make-empty-trie)
      t))

; bind : (list base) object trie -> trie
;   extend the trie t with a binding of the key 
;   sequence ks to the value x
(define (trie-bind ks x t)
  (cond
    [(null? ks) 
     (trie #t x (trie-map t))]
    [else
     (let ([k  (car ks)]
           [ks (cdr ks)]
           [m  (trie-map t)]
           [v  (trie-value t)]
           [e  (trie-end t)])
       (let* ([t  (handle-not-found (fm:lookup k m))]
              [t1 (trie-bind ks x t)])
         (trie e v (fm:bind k t1 m))))]))

; lookup : (list base) trie -> (union value #f)
;   return either the value associated to the key sequence ks,
;   or return #f if no association is found
(define (trie-lookup ks t)
  (cond
    [(and (null? ks) (not (trie-end t)))
     #f] ; not found
    [(null? ks)
     (trie-value t)]
    [else
     (trie-lookup (cdr ks) 
                  (handle-not-found (fm:lookup (car ks) (trie-map t))))]))


;;; TEST

(define (insert-word w v t)
  (trie-bind (string->list w) v t))

(define (lookup-word w t)
  (trie-lookup (string->list w) t))

(define (chop s)
  (substring s 0 (sub1 (string-length s))))

(define (legal? w)
  (and (= (string-length w) 3)
       (for/and ([c w])
         (char-alphabetic? c))))

(define (insert-word-if-legal w t)
  (let ([w (string-downcase w)])
    (if (legal? w)
        (insert-word w #t t)
        t)))



; trie-keys : trie -> (list (list base))
;   return list of all keys in trie
(define (trie-keys t)
  (define ends-here
    (for/list ([a (trie-map t)] #:when (trie-end (cdr a)))
      (list (car a))))
  (define longer
    (append*
     (for/list ([a (trie-map t)])
       (define k  (car a))
       (define tk (cdr a))
       (map (λ (ks) (cons k ks))
            (trie-keys tk)))))
  (append ends-here longer))


(define (trie-completions ks t)
  ; return list of all completions of ks in t
  (cond
    [(and (null? ks) (not (trie-end t)))
     (trie-keys t)]
    [(null? ks)
     (cons '() (trie-keys t))]
    [else
     (map (λ (key) (cons (car ks) key))
          (trie-completions (cdr ks) 
                            (handle-not-found (fm:lookup (car ks) (trie-map t)))))]))



(define (make-empty-completions)
  (make-empty-trie))

(define (completions-insert-string t s)
  (insert-word s 'ignore t))

(define (completions-lookup t s)
  (map list->string 
       (trie-completions (string->list s) t)))


#;(begin
(trie-keys test-trie)

(define test-trie
  (insert-word "swords" 5
               (insert-word "sword" 4
                            (insert-word "o" 3
                                         (insert-word "ca" 2 
                                                      (insert-word "cat" 1 (make-empty-trie))))))))





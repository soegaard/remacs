#lang racket
(module+ test (require rackunit) (random-seed 42))
;;;
;;; This is a functional version of the algorithm
;;; described in:
;;;     Randomized Binary Search Tree
;;;     Conrado Martinez and Salvador Roura
;;;     http://www.cis.temple.edu/~wolfgang/cis551/martinez.pdf

; insert : tree index element -> tree      O(log(n))
; delete : tree index         -> tree      O(log(n))
; ref    : tree index         -> element   O(log(n))
; size   : tree               -> integer   O(1)
; split  : tree index         -> tree tree O(log(n))

;;; Representation

; An empty tree is represented as #f
; Non-empty trees are represented as:
(struct tree (n l x r) #:transparent)
;   n = size = size(l) + 1 + size(r)
;   l = left subtree
;   r = right subtree
;   x = element

;;; Invariant 
;   n = size(l) + 1 + size(r)

;;; Construction

; All non-empty trees are constructed using make,
; which takes care of the invariant.

; make : tree element tree -> tree
(define (make l x r)
  (tree (+ (size l) 1 (size r))
        l x r))

;;; Accessors and predicates

(define left     tree-l)
(define right    tree-r)
(define element  tree-x)

(define (empty-tree? t) (not t))
(define (size t) (if t (tree-n t) 0))

;;; Conversion to list

; elements : tree -> list-of-elements
;   return the list of elements in an inorder traversal
(define (elements t)
  (if t
      (append (elements (left t)) (cons (element t) (elements (right t))))
      '()))

(module+ test
  ; a few sample trees
  (define t-a  (make #f 'a #f))
  (define t-b  (make #f 'b #f))
  (define t-ab (make t-a 'b #f))
  ; elements
  (check-equal? (elements #f) '())
  (check-equal? (elements t-a)  '(a))
  (check-equal? (elements t-b)  '(b))
  (check-equal? (elements t-ab) '(a b))
  ; build non-balanced tree
  (define (linear-tree xs)
    (for/fold ([t #f]) ([x xs])
      (make t x #f))))


;;; Retrival

; ref : tree index -> element
;   return the element at index i
(define (ref t i)
  (match t
    [#f             (error 'ref "got empty tree")]
    [(tree n l x r) (define sl (size l))
                    (cond
                      [(>= i n) (error 'ref (~a "index " i " is to large for a tree with " 
                                                n " elements."))]
                      [(= i sl) x]
                      [(< i sl) (ref l i)]
                      [else     (ref r (- i sl 1))])]))

;;; Insertion

; insert : element index tree -> tree
;   insert the element x at index i in tree t
(define (insert t i x)
  (match t
    [#f             (make #f x #f)]
    [(tree n l y r) (define root? (= (random (+ n 1)) n))
                    (define sl (size l))
                    (cond 
                      [root?     (insert-at-root t i x)]
                      [(<= i sl) (make (insert l i x) y r)]
                      [else      (make l y (insert r i x))])]
    [_ (error 'insert (~a "got " x " " i " " t))]))

(module+ test 
  (check-equal? (insert #f 0 'a) t-a)
  (check-equal? (insert #f 1 'a) t-a)
  (check-equal? (insert #f 10 'a) t-a)
  (check-equal? (elements (insert (insert #f 0 'a) 0 'b)) '(b a))
  (check-equal? (elements (insert (insert #f 0 'b) 0 'a)) '(a b)))


; insert-at-root : tree index element -> tree
;   insert the element x at index i in the tree,
;   make sure x becomes the new root
(define (insert-at-root t i x)
  (define-values (t< t>) (split t i))
  (make t< x t>))

(module+ test
  (check-true (let ([t (insert-at-root (insert #f 0 'a) 0 'b)])
                (and (eq? (element t) 'b) (equal? (elements t) '(b a)))))
  (check-true (let ([t (insert-at-root (insert #f 0 'a) 1 'b)])
                (and (eq? (element t) 'b) (equal? (elements t) '(a b))))))

; split : tree index -> tree tree
;   return two trees, the first one contains all elements with index
;   smaller or equal to i. The other contains the remaining elements.
(define (split t i)
  (match t
    [#f             (values #f #f)]
    [(tree n l y r) (define sl (size l))
                    (cond
                      [(<= i sl) (define-values (l< l>) (split l i))
                                 (values l< (make l> y r))]
                      [else      (define j (- i 1 sl))
                                 (define-values (r< r>) (split r j))
                                 (values (make l y r<) r>)])]))

(module+ test
  (check-equal? (elements (linear-tree (range 10))) (range 10))
  (check-equal? (let-values ([(t s) (split t-a  0)]) (list t s)) (list #f t-a))
  (check-equal? (let-values ([(t s) (split t-a  1)]) (list t s)) (list t-a #f))
  (check-equal? (let-values ([(t s) (split t-ab 0)]) (list t s)) (list #f t-ab))
  (check-equal? (let-values ([(t s) (split t-ab 1)]) (list t s)) (list t-a t-b))
  (check-equal? (let-values ([(t s) (split t-ab 2)]) (list t s)) (list t-ab #f))
  (define a..m '(a b c d e f g h i j k l m))
  (check-equal? (elements (for/fold ([t #f]) ([x (reverse a..m)]) (insert t 0 x))) a..m))


;;; Deletion

; delete : tree index -> tree
;   return new tree where element i is removed
(define (delete t i)
  (match t
    [#f             #f]
    [(tree n l x r) (define sl (size l))
                    (cond
                      [(= i sl) (join l r)]
                      [(< i sl) (make (delete l i) x r)]
                      [else     (make l x (delete r (- i sl 1)))])]))

; join : tree tree -> tree
;   All elements in tree l must smaller than all elements in r
;   The root of the joined tree is randomly chosen to be either
;   the root of or the root of r.
(define (join l r)
  (define m (size l))
  (define n (size r))
  (define total (+ m n))
  (if (= total 0)
      #f
      (if (< (random total) m)
          (make (left l) (element l) (join (right l) r))
          (make (join l (left r)) (element r) (right r)))))

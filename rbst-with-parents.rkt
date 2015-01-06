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
(struct tree (n l x r p) #:transparent #:mutable)
;   n = size = size(l) + 1 + size(r)
;   l = left subtree   
;   r = right subtree  
;   x = element
;   p = parent

;;; Invariant 
;   n = size(l) + 1 + size(r)

;;; Construction

; All non-empty trees are constructed using make,
; which takes care of the invariant.

; make : tree element tree -> tree
(define (make l x r p)
  (define t (tree (+ (size l) 1 (size r))
                  l x r p))
  ; not t is the parent of l and r
  (and l (set-tree-p! l t))
  (and r (set-tree-p! r t))
  t)
  
        

;;; Accessors and predicates

(define left     tree-l)
(define right    tree-r)
(define element  tree-x)
(define parent   tree-p)

(define (size t) (if t (tree-n t) 0))

; The root of a non-empty tree has no parent.
(define (root? t)
  (not (parent t)))

;;; Conversion to list

; elements : tree -> list-of-elements
;   return the list of elements in an inorder traversal
(define (elements t)
  (if t
      (append (elements (left t)) (cons (element t) (elements (right t))))
      '()))

(module+ test
  ; a few sample trees
  (define t-a  (make #f 'a #f #f))
  (define t-b  (make #f 'b #f #f))
  (define t-ab (make t-a 'b #f #f))
  ; elements
  (check-equal? (elements #f) '())
  (check-equal? (elements t-a)  '(a))
  (check-equal? (elements t-b)  '(b))
  (check-equal? (elements t-ab) '(a b))
  ; build non-balanced tree
  (define (linear-tree xs)
    (for/fold ([t #f]) ([x xs])
      (make t x #f #f))))


;;; Retrival

; ref : tree index -> element
;   return the element at index i
;   if e is true, the element is returned,
;   if e is false, the tree in which the i'th element is root is returned
(define (ref t i [e #t])
  (match t
    [#f               (error 'ref "got empty tree")]
    [(tree n l x r _) (define sl (size l))
                      (cond
                        [(>= i n) (error 'ref (~a "index " i " is to large for a tree with " 
                                                  n " elements."))]
                        [(= i sl) (if e x t)]
                        [(< i sl) (ref l i e)]
                        [else     (ref r (- i sl 1) e)])]))

;;; Insertion

; insert : element index tree -> tree
;   insert the element x at index i in tree t
(define (insert t i x)
  (match t
    [#f               (make #f x #f #f)]
    [(tree n l y r p) (define root? (= (random (+ n 1)) n))
                      (define sl (size l))
                      (cond 
                        [root?     (insert-at-root t i x)]
                        [(<= i sl) (make (insert l i x) y r p)]
                        [else      (make l y (insert r i x) p)])]
    [_ (error 'insert (~a "got " x " " i " " t))]))

(module+ test 
  (check-equal? (elements (insert #f 0 'a))  '(a))
  (check-equal? (elements (insert #f 1 'a))  '(a)) ; xxx
  (check-equal? (elements (insert #f 10 'a)) '(a)) ; xxx
  (check-equal? (elements (insert (insert #f 0 'a) 0 'b)) '(b a))
  (check-equal? (elements (insert (insert #f 0 'b) 0 'a)) '(a b)))


; insert-at-root : tree index element -> tree
;   insert the element x at index i in the tree,
;   make sure x becomes the new root
(define (insert-at-root t i x)
  (define-values (t< t>) (split t i))
  (make t< x t> #f))

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
    [#f               (values #f #f)]
    [(tree n l y r p) (define sl (size l))
                      (cond
                        [(<= i sl) (define-values (l< l>) (split l i))
                                   (values l< (make l> y r #f))]
                        [else      (define j (- i 1 sl))
                                   (define-values (r< r>) (split r j))
                                   (values (make l y r< #f) r>)])]))

(module+ test
  (check-equal? (elements (linear-tree (range 10))) (range 10))
  (check-equal? (let-values ([(t s) (split t-a  0)]) (map elements (list t s))) '(() (a)))
  (check-equal? (let-values ([(t s) (split t-a  1)]) (map elements (list t s))) '((a) ()))
  (check-equal? (let-values ([(t s) (split t-ab 0)]) (map elements (list t s))) '(() (a b)))
  (check-equal? (let-values ([(t s) (split t-ab 1)]) (map elements (list t s))) '((a) (b)))
  (check-equal? (let-values ([(t s) (split t-ab 2)]) (map elements (list t s))) '((a b) ()))
  (define a..m '(a b c d e f g h i j k l m))
  (check-equal? (elements (for/fold ([t #f]) ([x (reverse a..m)]) (insert t 0 x))) a..m))


;;; Deletion

; delete : tree index -> tree
;   return new tree where element i is removed
(define (delete t i)
  (match t
    [#f               #f]
    [(tree n l x r p) (define sl (size l))
                      (cond
                        [(= i sl) (join l r p)]
                        [(< i sl) (make (delete l i) x r p)]
                        [else     (make l x (delete r (- i sl 1)) p)])]))

; join : tree tree -> tree
;   All elements in tree l must smaller than all elements in r
;   The root of the joined tree is randomly chosen to be either
;   the root of or the root of r.
(define (join l r p)
  (define m (size l))
  (define n (size r))
  (define total (+ m n))
  (if (= total 0)
      #f
      (if (< (random total) m)
          (make (left l) (element l) (join (right l) r) p)
          (make (join l (left r)) (element r) (right r) p))))




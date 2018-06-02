#lang racket
(require "buffer.rkt"
         "text.rkt"
         "line.rkt"
         "parameters.rkt"
         "representation.rkt"
         "dlist.rkt")


;;;
;;; LINES
;;;

(module+ test
  (define illead-text 
    (new-text 
     (list->lines
      (list "x\n"))))
  #;(define illead-text 
      (new-text 
       (list->lines
        (list "Sing, O goddess, the anger of Achilles son of Peleus, that brought\n"
              "countless ills upon the Achaeans. Many a brave soul did it send hurrying\n"
              "down to Hades, and many a hero did it yield a prey to dogs and vultures,\n"
              "for so were the counsels of Jove fulfilled from the day on which the\n"
              "son of Atreus, king of men, and great Achilles, first fell out with\n"
              "one another.\n"))))  
  
  ; recreate the same text file from scratch
  (define (create-new-test-file path)
    (with-output-to-file path
      (λ() (for ([line (text-lines illead-text)])
             (for ([s (line-strings line)])
               (display s))))
      #:exists 'replace)))

;;;
;;; TEXT
;;;

(module+ test
  (void (create-new-test-file "illead.txt"))
  ; (displayln "--- illead test file ---")
  ; (write (path->text "illead.txt")) (newline)
  ; (displayln "---")
  ; (write illead-text) (newline)
  ; (displayln "---")
  #;(check-equal? (path->text "illead.txt") illead-text))


;;;
;;; BUFFER
;;;

(module+ test
  (provide illead-buffer)
  (define illead-buffer (new-buffer illead-text "illead.txt" (generate-new-buffer-name "illead")))
  (save-buffer! illead-buffer)
  #;(check-equal? (path->text "illead.txt") illead-text))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define b (new-buffer (new-text) "illead.txt" (generate-new-buffer-name "illead")))
  (read-buffer! b)
  #;(check-equal? b illead-buffer))

(module+ test
  (void (create-new-test-file "illead.txt"))
  (define append-buffer (new-buffer (new-text)))
  (append-to-buffer-from-file append-buffer "illead.txt")
  (append-to-buffer-from-file append-buffer "illead.txt")
  (save-buffer! b) ; make sure the buffer is unmodified before comparison
  #;(check-equal? (buffer-text append-buffer) (text-append! illead-text illead-text)))

#;(module+ test
  (define ib illead-buffer)
  ;(current-buffer ib)
  (current-buffer scratch-buffer)
  (define f  (frame #f #f #f #f #f))
  (frame-install-frame%! f) ; installs frame% and panel
  
  (define p (frame-panel f))
  (define w (new-window f p scratch-buffer 'root))
  
  ;(define sp (vertical-split-window f #f #f #f #f #f #f))  
  ; (define w  (window f #f c sp ib))
  ; (define c2 #f)
  ; (define w2 (window f #f c2 sp (get-buffer "*scratch*")))
  ; (set-vertical-split-window-above! sp w)
  ; (set-vertical-split-window-below! sp w2)
  ; (set-frame-windows! f sp)
  
  (set-frame-windows! f w)
  (current-window w)
  (current-frame f)
  
  (send (window-canvas w) focus))
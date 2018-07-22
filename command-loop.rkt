#lang racket/base
(provide queue-command
         (rename-out [send-command raw-send-command])
         send-command
         (rename-out [send-command send-command/parameters])
         start-command-loop)

;;;
;;; COMMAND LOOP
;;; 

; In order to avoid race conditions the event handlers in the gui
; (menu choices, key events, mouse events etc) aren't allowed
; to call buffer operations directly. Instead they can use the
; command queue to enter thunks. The thunks will be run by the
; the command loop.

; Note that most commands expect the parameters
;   current-buffer, current-window, current-frame to be set correctly.
; Therefore use the macro send-command

(require (for-syntax racket/base syntax/parse)
         racket/async-channel
         "parameters.rkt"
         "locals.rkt")

(define command-channel (make-async-channel))

(define (queue-command cmd)
  (async-channel-put command-channel cmd))

(define (start-command-loop)
  (thread
   (λ ()
     (let loop ()
       (define cmd (async-channel-get command-channel))
       (cmd)
       (loop)))))

(define-syntax (send-command stx)
  (syntax-parse stx
    [(_send-command expr ...)
     (syntax/loc stx
       #;(let () expr ...)
       (queue-command
        (λ ()
          (display "." (current-error-port))
          expr ...)))]))

#;(define-syntax (send-command/parameters stx)
  (syntax-parse stx
    [(_send-command (param ...) expr ...)
     (with-syntax ([(p ...) (generate-temporaries #'(param ...))])
       (syntax/loc stx
         (let ([p (param)] ...)
           (queue-command
            (λ ()
              (localize ([param p] ...)
                expr ...))))))]))

#;(define-syntax (raw-send-command stx)
  (syntax-parse stx
    [(_rawsend-command expr ...)
     (syntax/loc stx
       (queue-command
        (λ ()
          expr ...)))]))


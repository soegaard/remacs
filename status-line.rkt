#lang racket/base
(provide status-line-hook
         status-line-time
         status-line-coloring-time
         status-line-command-time
         status-line-render-time
         status-line-show-paren-time)
;;;
;;; STATUS LINE
;;;

; Each frame (GUI window) has a status line - a horizontal line at
; the bottom of the frame (under the displayed buffer). The status
; line is used to display information on the current buffer.

; The default information displayed is:
;   - name of the current buffer
;   - the row and column of the point 
;   - the position of the point
;   - the current major mode
;   - the time used to run the last commands (measured in milliseconds)

(require racket/format
         "buffer-locals.rkt"
         "mark.rkt"
         "mode.rkt"
         "parameters.rkt"
         "representation.rkt")

; The variable the-time keeps track of the time used by the last command.
(define the-time 0)
(define (status-line-time t) (set! the-time t))

(define the-render-time 0)
(define (status-line-render-time t) (set! the-render-time t))

(define the-coloring-time 0)
(define (status-line-coloring-time t) (set! the-coloring-time t))

(define the-command-time 0)
(define (status-line-command-time t) (set! the-command-time t))

(define the-show-paren-time 0)
(define (status-line-show-paren-time t) (set! the-show-paren-time t))

(define (position pos)
  (if (mark? pos) (mark-position pos) pos))

; The default function used to compute status line information
(define (status-line-hook)
  (define w     (current-window))
  (define b     (window-buffer w))  
  (define point (buffer-point b))
  (cond
    [b (define save-status      (if (buffer-modified? b) "***" "---"))
       (define line?            (local   line-number-mode?))
       (define col?             (local column-number-mode?))
       (define-values (row col) (if (and (<= (buffer-length b) (local line-number-display-limit)))
                                    (mark-row+column point)
                                    (values "-" "-")))
       (define line+col         (cond [(and line? col?) (~a "(" row "," col ")")]
                                      [line?            (~a "L" row)]
                                      [col?             (~a "C" col)]
                                      [else             ""]))
       (~a save-status  
           "   " "Buffer: "          (buffer-name) "    " line+col
           "   " "Position: "        (mark-position point)
           "   " "Length: "          (buffer-length b)
           "   " "Mode: "            "(" (get-mode-name) ")"
           "   " "Time: "            the-time
           "   " "Command Time:"     the-command-time
           "   " "Render Time: "     the-render-time
           "   " "Coloring Time: "   the-coloring-time
           "   " "Show Paren Time: " the-show-paren-time
           ; "   " "Window: "          (position (window-start-mark w)) "-"
           ;                          (position (window-end-mark w))
           )]
    [else
     "No current buffer"]))

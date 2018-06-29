#lang racket/base
(provide status-line-hook
         status-line-time
         status-line-render-time)
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

; The default function used to compute status line information
(define (status-line-hook)
  (define b (current-buffer))
  (cond
    [b (define save-status      (if (buffer-modified? b) "***" "---"))
       (define line?            (local   line-number-mode?))
       (define col?             (local column-number-mode?))
       (define-values (row col) (if (and (<= (buffer-length b) (local line-number-display-limit)))
                                    (mark-row+column (buffer-point b))
                                    (values "-" "-")))
       (define line+col         (cond [(and line? col?) (~a "(" row "," col ")")]
                                      [line?            (~a "L" row)]
                                      [col?             (~a "C" col)]
                                      [else             ""]))
       (~a save-status  
           "   " "Buffer: "          (buffer-name) "    " line+col
           "   " "Position: "        (mark-position (buffer-point (current-buffer)))
           "   " "Length: "          (buffer-length (current-buffer))
           "   " "Mode: "            "(" (get-mode-name) ")"
           "   " "Time: "            the-time
           "   " "Render Time: "     the-render-time)]
    [else
     "No current buffer"]))

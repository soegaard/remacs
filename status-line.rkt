#lang racket/base
(provide status-line-hook)

;;;
;;; STATUS LINE
;;;

(require racket/format
         "mark.rkt"
         "mode.rkt"
         "parameters.rkt"
         "representation.rkt")


; The status line is shown at the bottom of a buffer window.
(define (status-line-hook)
  (define b (current-buffer))
  (define-values (row col) (mark-row+column (buffer-point b)))
  (define save-status (if (buffer-modified? b) "***" "---"))
  (~a save-status  
      "   " "Buffer: "          (buffer-name) "    " "(" row "," col ")"
      "   " "Position: " (mark-position (buffer-point (current-buffer)))
      "   " "Length: "   (buffer-length (current-buffer))
      "   " "Mode: "     "(" (get-mode-name) ")"))






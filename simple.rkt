#lang racket/base
(provide (all-defined-out))

;;;
;;; SIMPLE 
;;;

; This file contains various general editing commands that
; don't belong in a specific major mode.

(require racket/class racket/list
         racket/gui/base
         framework
         "buffer.rkt"
         "commands.rkt"
         "frame.rkt"
         "mark.rkt"
         "parameters.rkt"
         "point.rkt"
         "region.rkt"
         "representation.rkt"
         "render.rkt"
         "window.rkt")


(define-interactive (beginning-of-line)   (buffer-move-point-to-beginning-of-line! (current-buffer)))
(define-interactive (end-of-line)         (buffer-move-point-to-end-of-line!       (current-buffer)))

(define-interactive (move-to-column n)    (buffer-move-to-column!  (current-buffer) n)) ; n=numprefix 

(define-interactive (backward-char)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point! (current-buffer) -1))
(define-interactive (forward-char [b (current-buffer)])
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point! b +1))
(define-interactive (previous-line)       
  (cond [(region-mark) => mark-deactivate!])
  (buffer-move-point-up!   (current-buffer)))

(define-interactive (forward-line)
  ; this moves an entire text-line (see next-line for moving a screen line)
  (cond [(region-mark) => mark-deactivate!])
  (define b (current-buffer))
  (if (mark-on-last-line? (buffer-point b))
      (buffer-move-point-to-end-of-line! b)
      (buffer-move-point-down! b)))
(define-interactive (next-line)
  ; this moves a screen line 
  (cond [(region-mark) => mark-deactivate!])
  (define b (current-buffer))
  (if (mark-on-last-line? (buffer-point b))      
      (buffer-move-point-to-end-of-line! b)
      (buffer-move-point-down! b)))
(define-interactive (backward-word)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-backward-word!   (current-buffer)))
(define-interactive (forward-word)
  (cond [(region-mark) => mark-deactivate!])
  (buffer-forward-word!   (current-buffer)))

(define-interactive (command-set-mark)
  (buffer-set-mark-to-point (current-buffer)))

(define-interactive (exchange-point-and-mark)
  (unless (get-mark) (new-mark (current-buffer) "*mark*")) ; TODO
  (buffer-exchange-point-and-mark! (current-buffer))
  (mark-activate! (get-mark)))
(define-interactive (mark-word) ; Set mark after next word (doesn't move point)
  (define m (get-mark))
  (define swap exchange-point-and-mark)
  (cond [(and m (mark-active? m))
         (with-saved-point
             (cond
               [(mark<= (get-point) m) (swap) (forward-word)  (swap)]
               [else                   (swap) (backward-word) (swap)]))]
        [else              (command-set-mark) (mark-word)]))


(define-interactive (page-down [w (current-window)])
  (define point                (buffer-point (window-buffer w)))
  (define start-mark           (window-start-mark w))
  (define end-mark             (window-end-mark w))
  (define-values (start-row _) (mark-row+column start-mark))
  (define-values (end-row  __) (mark-row+column end-mark))  
  (define delta                (max 0 (- (number-of-lines-on-screen w)
                                         (current-next-screen-context-lines))))
  (mark-move-down! point      delta)
  (mark-move-down! start-mark delta)
  (mark-move-down! end-mark   delta)
  (refresh-frame))
(define-interactive (page-up [w (current-window)])
  (define point                (buffer-point (window-buffer w)))
  (define start-mark           (window-start-mark w))
  (define end-mark             (window-end-mark w))
  (define-values (start-row _) (mark-row+column start-mark))
  (define-values (end-row  __) (mark-row+column end-mark))  
  (define delta                (max 0 (- (number-of-lines-on-screen w)
                                         (current-next-screen-context-lines))))
  (mark-move-up! point      delta)
  (mark-move-up! start-mark delta)  
  (mark-move-up! end-mark   delta)
  (refresh-frame))


(define (prepare-extend-region)
  (define marks (buffer-marks (current-buffer)))
  (cond [(and (not (empty? marks)) (not (mark-active? (first marks))))
         (delete-mark! (first marks))
         (command-set-mark)]
        [(empty? marks)
         (command-set-mark)])
  (mark-activate! (region-mark)))

(define-interactive (backward-char/extend-region)
  (prepare-extend-region)
  (buffer-move-point! (current-buffer) -1))
(define-interactive (forward-char/extend-region)
  (prepare-extend-region)
  (buffer-move-point! (current-buffer) +1))
(define-interactive (previous-line/extend-region) 
  (prepare-extend-region)
  (buffer-move-point-up! (current-buffer)))
(define-interactive (next-line/extend-region)
  (prepare-extend-region)
  (define b (current-buffer))
  (if (mark-on-last-line? (buffer-point b))
      (buffer-move-point-to-end-of-line! b)
      (buffer-move-point-down! b)))
(define-interactive (forward-word/extend-region) 
  (prepare-extend-region)
  (buffer-forward-word! (current-buffer)))
(define-interactive (backward-word/extend-region)
  (prepare-extend-region)
  (buffer-backward-word! (current-buffer)))
(define-interactive (beginning-of-line/extend-region)
  (prepare-extend-region)
  (buffer-move-point-to-beginning-of-line! (current-buffer)))
(define-interactive (end-of-line/extend-region)
  (prepare-extend-region)
  (buffer-move-point-to-end-of-line! (current-buffer)))

(define-interactive (save-buffer)         (save-buffer!    (current-buffer)) (refresh-frame))
(define-interactive (save-buffer-as)      (save-buffer-as! (current-buffer)) (refresh-frame))
(define-interactive (save-some-buffers)   (save-buffer)) ; todo : ask in minibuffer
(define-interactive (beginning-of-buffer [b (current-buffer)]) (buffer-move-point-to-position! b 0))
(define-interactive (end-of-buffer       [b (current-buffer)])
  (buffer-move-point-to-position! b (- (buffer-length b) 1)))
(define-interactive (end-of-buffer/extend-region)
  (prepare-extend-region)
  (end-of-buffer (current-buffer)))
(define-interactive (beginning-of-buffer/extend-region)
  (prepare-extend-region)
  (beginning-of-buffer (current-buffer)))

(define-interactive (open-file-or-create [path (finder:get-file)])
  (when path ; #f = none selected
    (define b (buffer-open-file-or-create path))
    (set-window-buffer! (current-window) b)
    (current-buffer b)
    (refresh-frame (current-frame))))

(define-interactive (next-buffer) ; show next buffer in current window
  (define w (current-window))
  (define b (get-next-buffer))
  (set-window-buffer! w b)
  (current-buffer b))

(define-interactive (previous-buffer) ; show next buffer in current window
  (define w (current-window))
  (define b (get-previous-buffer))
  (set-window-buffer! w b)
  (current-buffer b))

(define-interactive (other-window) ; switch current window and buffer
  (define ws (frame-window-tree (current-frame)))
  (define w (list-next ws (current-window) eq?))
  (current-window w)
  (current-buffer (window-buffer w))
  (send (window-canvas w) focus))

(define-interactive (delete-window [w (current-window)])
  (window-delete! w))

(define-interactive (delete-other-windows [w (current-window)])
  (define ws (frame-window-tree (window-frame w)))
  (for ([win (in-list ws)])
    (unless (eq? w win)
      (delete-window win)))
  (refresh-frame))

(define-interactive (maximize-frame [f (current-frame)]) ; maximize / demaximize frame
  (when (frame? f)
    (define f% (frame-frame% f))
    (when (is-a? f% frame%)
      (send f% maximize (not (send f% is-maximized?))))))


(define (position->index pos)
  (if (mark? pos) (mark-position pos) pos))

(define (check-position who what)
  (unless (or (number? what) (mark? what))
    (error who "expected a position (index or mark), got ~a" what)))

(define-interactive (goto-char pos)
  (check-position 'goto-char pos)
  ; todo: add narrowing
  (buffer-move-point-to-position! (current-buffer) (position->index pos)))

(define-interactive (set-mark pos)
  (check-position 'set-mark pos)
  (with-saved-point
      (goto-char pos)
      (mark-activate!
       (buffer-set-mark-to-point))))



(define-interactive (text-scale-adjust m)
  (displayln `(text-scale-adjust ,m))
  (font-size (min 40 (max 1 (+ (font-size) m)))))

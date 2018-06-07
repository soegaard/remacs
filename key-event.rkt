#lang racket/base
(provide key-event->key)

(require racket/class racket/format racket/match)

(define (key-event->key event)
  #;(newline)
  #;(begin
      (write (list 'key-event->key
                   'key                (send event get-key-code)
                   'other-shift        (send event get-other-shift-key-code)
                   'other-altgr        (send event get-other-altgr-key-code)
                   'other-shift-altgr  (send event get-other-shift-altgr-key-code)
                   'other-caps         (send event get-other-caps-key-code)))
      (newline))
  (define shift? (send event get-shift-down))
  (define alt?   (send event get-alt-down))
  (define ctrl?  (send event get-control-down))
  (define caps?  (send event get-caps-down))
  (define cmd?   (case (system-type 'os)
                   ; racket reports cmd down as meta down
                   [(macosx) (send event get-meta-down)]
                   ; other systems do not have cmd
                   [else     #f]))  
  (define meta?  (case (system-type 'os)
                   ; use the alt key as meta
                   [(macosx) (send event get-alt-down)]
                   [else     (send event get-meta-down)]))    ; mac: cmd, pc: alt, unix: meta
  #;(displayln (list 'shift shift? 'alt alt? 'ctrl ctrl? 'meta meta? 'cmd cmd? 'caps caps?))
  
  (define c      (send event get-key-code))
  ; mapping caps lock to control, results in rcontrol instead of control (sigh)
  (set! c        (if (eq? c 'rcontrol) 'control c))
  ; k = key without modifier
  (define k      (cond
                   [(and ctrl? alt?)  c]
                   [cmd?              c]
                   [alt?              (or (and (symbol? c) c)
                                          (send event get-other-altgr-key-code))] ; OS X: 
                   [else              c]))
  
  (define alt-is-meta? #t)
  (define cmd-is-meta? #f)
  (define (alt   k) 
    (cond
      [alt-is-meta? k]
      [alt?         (~a "A-" k)]
      [else         k]))
  (define (ctrl  k)
    ; (displayln (list "k:" k))
    (if ctrl?  (~a "C-" k) k))
  (define (cmd   k) 
    (cond
      [cmd-is-meta? k]
      [cmd?         (~a "D-" k)]
      [else         k]))
  (define (meta  k) (if meta?  (~a "M-" k) k))
  (define (shift k) (if shift? (~a "S-" k) k))
  
  (let ([k (match k 
             ['escape     "ESC"] 
             [#\backspace "backspace"]
             [#\return    "return"]
             [#\space     "space"]
             [_ k])])
    (cond 
      [(eq? k 'control)       'control]  ; ignore  control + nothing
      [(eq? k 'rcontrol)      'rcontrol] ; ignore rcontrol + nothing
      [(and (symbol? c) meta? shift?) (~a "M-S-" k)]
      [(or ctrl? alt? meta? cmd?)     (alt (ctrl (cmd (meta (shift k)))))]
      [(and shift? (eq? k 'shift))    'shift]
      [(and shift? (symbol? k))       (~a "S-" k)]
      [else                           k])))

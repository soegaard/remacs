#lang racket
(provide (struct-out line)
         (struct-out overlay)
         (struct-out property)
         (struct-out linked-line)
         (struct-out text)
         position-row+column
         row+column->position
         (struct-out stats)
         (struct-out buffer)
         buffer-dirty!
         buffer-length
         buffer-modified?
         set-buffer-modified!
         buffer-name
         buffer-point
         (struct-out mark)
         mark-row+column
         (struct-out mode)
         (struct-out window)
         (struct-out frame)
         (struct-out split-window)
         (struct-out horizontal-split-window)
         (struct-out vertical-split-window))

(require "dlist.rkt" "parameters.rkt")

;;;
;;; REPRESENTATION
;;;

(struct line (strings length) #:transparent #:mutable)
; A line is a list of elements of the types:
;   string        represents actual text
;   property      represents a text property e.g. bold
;   overlay       represents ...

; properties are copied as part of the text
; overlays are not copied - they are specifically not part of the text
(struct overlay  (specification) #:transparent)
(struct property (specification) #:transparent)

(struct linked-line dcons (version marks) #:transparent #:mutable)
; the element of a linked-line is a line struct
; marks is a set of marks located on the line
; version will be used for the redisplay code

(struct text (lines length) #:transparent #:mutable)
; A text being edited is represented as a doubly linked list of lines.

(struct stats (num-lines num-chars) #:transparent)
; The number of lines and number of characters in a text.

#;(struct buffer (text name path points marks modes cur-line num-chars num-lines modified? locals))
(module buffer-struct racket/base
  ; buffer-name and buffer-modified? are extendeded to handle current-buffer later on
  (provide (except-out (struct-out buffer) buffer-name buffer-modified?) 
           (rename-out [buffer-name -buffer-name] [buffer-modified? -buffer-modified?]))
  (struct buffer (text name path points marks modes cur-line num-chars num-lines modified? locals)
    #:transparent #:mutable))
(require (submod "." buffer-struct))
; A buffer is the basic unit of text being edited.
; It contains a text being edited.
; The buffer has a name, so the user can refer to the buffer.
; The buffer might have an associated file:
;   path = #f     <=>  buffer is not associated with a file
;   path = path   <=>  reads and writes to the file given by the path
; A point is a position between two characters. 
; Insertions and deletion will happen at the points (usually only one).
; If modified? is true, then the buffer has been modified since the last
; read or save of the file.
; The list modes contains the active modes (see below).
; marks = list of marks
; A buffer can have multiple marks:
; 

(struct mark (buffer link position name fixed? active?) #:transparent #:mutable)
; A mark rembers a position in the text of a buffer.
; The mark stores the link (linked-line) which hold the line.
; The mark name can be used to refer to the mark.
; fixed? = #f  A normal mark moves when insertions are made to the buffer.
; fixed? = #t  A fixed-mark remain in place.
; active? =#t  text between mark and point is an active region 
; active? =#f  region is non-active (will not be highlighted)

; If there is exactly one mark, the area between the point and the mark
; is called a region.

(struct mode (name) #:transparent)
; A mode has a name (displayed in the status bar).

(struct window (frame panel borders canvas parent buffer start-mark end-mark) #:mutable #:transparent)
; A window is an area in which a buffer is displayed.
; Multiple windows are grouped in a frame.
; Split windows are backed by a panel.
; Single windows are backed by a canvas into which the buffer is rendered.
; borders is a set of symbols indicating which borders to draw
;   'left 'right 'top 'bottom
; start = mark of first position in buffer to display
; end   = mark of last posistion in buffer to display
; The marks start and end are updated on redisplay such that
; the point is visible.

(struct frame (frame% panel windows mini-window status-line) #:mutable #:transparent)
; frame%      = gui frame 
; panel       = gui panel holding panels/canvases of the windows in frame
; windows     = split-window or window
; mini-window = ?
; status-line = status-line% at bottom of frame%

; A frame contains one or multiple windows.

(struct            split-window       window ()            #:mutable #:transparent)
(struct horizontal-split-window split-window (left  right) #:mutable #:transparent)
(struct   vertical-split-window split-window (above below) #:mutable #:transparent)

; The buffer of a split window is #f.

;;;
;;;
;;;

; buffer-point : buffer -> mark
;   return the first mark in the list of points
(define (buffer-point b)
  (first (buffer-points b)))

; buffer-length : buffer -> natural
;   return the total length of the text
(define (buffer-length b)
  (text-length (buffer-text b)))

; buffer-name : [buffer] -> string
;   return name of buffer
(define (buffer-name [b (current-buffer)]) 
  (-buffer-name b))

(define (buffer-modified? [b (current-buffer)])
  (-buffer-modified? b))

(define (buffer-dirty! [b (current-buffer)])
  (set-buffer-modified?! b #t))

; set-buffer-modified! : any [buffer] -> void
;   set modified?, redisplay mode line
(define (set-buffer-modified! flag [b (current-buffer)])
  ; TODO (redisplay-mode-line-for-current-buffer]
  (when flag (set-buffer-modified?! b #t)))


; position-row+column : text position -> integer integer
;   return row and column number of the position (index)
(define (position-row+column t p)
  (let/ec return
    (for/fold ([r 0] [q 0]) ([l (text-lines t)])
      ; q is the first position on line r
      (define n (line-length l))
      (if (> (+ q n) p)
          (return r (- p q))
          (values (+ r 1) (+ q n))))))

; row+column->position : text integer integer -> position
(define (row+column->position t row col)
  (let/ec return
    (define-values (last-row last-pos)
      (for/fold ([r 0] [q 0]) ([l (text-lines t)])
        ; q is the first position on line r
        (define n (line-length l))
        (if (= row r)
            (return (+ q (min col (- n 1))))
            (values (+ r 1) (+ q n)))))
    ; since we got here, row is larger than the larges row index in text
    last-pos))

; mark-row+column : mark- > integer integer
;   return row and column number for the mark m
(define (mark-row+column m)
  (position-row+column (buffer-text (mark-buffer m))
                       (mark-position m)))

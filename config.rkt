#lang racket/base
;;;
;;; CONFIGURATION
;;;

; This file contains the default configuration of Remacs.
; Variables defined in this file are used to make the default namespace.

(provide (all-defined-out))

(require racket/set racket/runtime-path
         "colors.rkt"
         "parameters.rkt")

(define-runtime-path simple.rkt "simple.rkt")

;;;
;;; MODES
;;;

; Set the initial mode to the fundamental mode.
(define major-mode      'fundamental-mode)
(define mode-name       "Fundamental")
(define color-buffer    #f)

;;;
;;; EDITING
;;;

;;; Indentation

; Note! Tabs aren't supported in rendering yet...
(define indent-tab-mode #f) ; #t = use tabs for indent, #f = use spaces
(define indent-for-tab (dynamic-require simple.rkt 'insert-tab-as-spaces))
(define tab-width       4)
(define tab-stop-list  #f)                                     ; every tab-width position
;(define tab-stop-list  (for/list ([i (in-range 7 200 8)]) i)) ; positions of tab stops

;;; Margins
(define left-margin 0) ; used by break-line (newline), move-to-left-margin and set-fill-prefix

;;; Filling
(define auto-fill-mode?     #t)
(define auto-fill-function  #f) ; set when mode activated
(define auto-fill-chars     (set #\space))
(define fill-column         76)
(define fill-prefix         "")

;;;
;;; RENDERING
;;;

;;; Screen Line Length
; Lines longer than  screen-line-length  are wrapped into multiple screen lines.
(define screen-line-length  80)

;;; Line and Column Number Mode
(define line-number-mode?         #t)    ; use line-number-mode to toggle
(define column-number-mode?       #t)    ; use line-column-mode to toggle
(define line-number-display-limit 10000) ; only display line number if buffer length is smaller

; Highlight Line Containing Point
(define hl-line-mode?             #f)      ; use hl-line-mode to toggle
(define hl-line-mode-color        base00)  ; (base02 used for region high light)

;;; Colors
(define background-color         base03)   ; solarized dark mode
(define region-highlighted-color base01)   ; solarized dark mode 
(define text-color               base0)    ; solarized dark mode
(define border-color             base00)   ; ?
(define show-paren-color         base2)    ; background color between parens
(define show-paren-error-color   red)      ; color to show no mathcing paren
;;; Style and Weight
(define text-style               'normal)  
(define text-weight              'normal)


;;; Wrapped Lines

; Long text lines are wrapped into multiple screen lines.
; If  wrapped-line-indicator?  is true, then  wrapped-line-indicator  is displayed
; after each wrapped line.
(define wrapped-line-indicator?  #t)
(define wrapped-line-indicator   "↵")

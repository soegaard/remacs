#lang racket/base
(require "representation.rkt" "parameters.rkt" "buffer-locals.rkt")
(provide (all-defined-out))

;;;
;;; MODES
;;;

; Each buffer has a (single) major mode.
; The buffer-local variable  major-mode  holds a symbol representing the major mode.
; Example: the symbol 'fundamental-mode represents the fundamental mode.
; The parameter  current-default-major-mode  determines which major mode a
; new buffer will get as defaul (see new-buffer).


(define (get-major-mode [b (current-buffer)])
  (ref-buffer-local b 'major-mode))

(define (set-major-mode! mode-sym [b (current-buffer)])
  (set-buffer-local! b 'major-mode mode-sym))

;;;
;;; mode-name
;;;
; the "pretty" name shown in the mode line

(define (get-mode-name [b (current-buffer)])
  (ref-buffer-local b 'mode-name "-mode has no name-"))

(define (set-mode-name! name [b (current-buffer)])
  (set-buffer-local! b 'mode-name name))

; Standard variable names in a mode

; name-mode-hook   ; list of functions to call when the mode is started
; name-mode-keymap ; keymap overriding the global-keymap

; a way to add key bindings to a keymap, e.g
;   (define-key keymap "\C-j" 'newline-and-indent)

; auto-mode-list
;   the auto mode list contains an association between
;   file endings and major modes that are to be started automatically,
;   when a file with a given ending is loaded.
; (add-to-list 'auto-mode-alist '("\\.wpd\\'" . wpdl-mode))
; syntax table
; local (key)map

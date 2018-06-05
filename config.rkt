#lang racket/base
;;;
;;; CONFIGURATION
;;;

; This file contains the default configuration of Emacs.
; Variables defined in this file are used to make the default namespace.

(provide (all-defined-out))

(require "parameters.rkt")

;;;
;;; MODES
;;;

; Set the initial mode to the fundamental mode.
(define major-mode 'fundamental-mode)
(define mode-name  "Fundamental")

;;;
;;; RENDERING
;;;

;;; Screen Line Length
; Lines longer than  screen-line-length  are wrapped into multiple screen lines.
(define screen-line-length  80)


;;; Wrapped Lines

; Long text lines are wrapped into multiple screen lines.
; If  wrapped-line-indicator?  is true, then  wrapped-line-indicator  is displayed
; after each wrapped line.
(define wrapped-line-indicator?  #t)
(define wrapped-line-indicator   "↵")

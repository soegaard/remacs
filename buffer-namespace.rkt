#lang racket/base
;;;
;;; Buffer Namespace
;;;

; This file contains the namespace use as the namespace for new buffers.
; The default namespace is also defined here.

;;; Provided

(provide new-buffer-namespace    ; -> namespace
         new-default-namespace)  ; -> namespace

(require racket/runtime-path
         ; these needs
         "config.rkt"
         "parameters.rkt"
         ;"window.rkt"
         "parameters.rkt")

;;; Runtime paths

; Modules that rely on global state (such as a registry of "all buffers")
; must be instantiated only once - in order for the state to be shared.
; Instances of the modules below are therefore attcahed to the buffer local
; namespace. 
(define-runtime-path buffer.rkt        "buffer.rkt")
(define-runtime-path buffer-locals.rkt "buffer-locals.rkt")
(define-runtime-path config.rkt        "config.rkt")         ; for default only
(define-runtime-path parameters.rkt    "parameters.rkt")
;(define-runtime-path window.rkt        "window.rkt")

(define all-runtime-paths
  (list 'racket/base buffer.rkt parameters.rkt buffer-locals.rkt))

(define-namespace-anchor namespace-anchor)
(define this-ns (namespace-anchor->namespace namespace-anchor))

(define (namespace-attach-and-require rt-path ns)
  (displayln rt-path)
  (parameterize ([current-namespace ns])
    (namespace-attach-module this-ns rt-path ns)
    (namespace-require rt-path)))

(define (namespace-attach-and-require-all ns)
  (for ([rt-path all-runtime-paths])
    (namespace-attach-and-require rt-path ns)))
  

;;; Namespace Creation

; We need to get a reference to the modules instantiated here,
; so we can attach them the new namespace.


; new-buffer-namespace : -> namespace
;    Allocate a new namespace for a new buffer (see eval-buffer).
(define (new-buffer-namespace)  
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    ; Attach a module before requiring it.
    ; The module attachments are needed in order not to instantiate struct definitions multiple times.
    (namespace-attach-and-require-all ns)
    (namespace-attach-and-require buffer.rkt ns)
  ns))

; new-default-namespace : -> namespace
;    Make the (unique) default namespace.
;    This namespace holds all default values.
(define (new-default-namespace)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    ; Attach module before requiring it.
    ; The module attachments are needed in order not to instantiate struct definitions multiple times.
    ; (namespace-attach-and-require-all ns)
    (for ([rt-path (list 'racket/base parameters.rkt config.rkt)])
      (namespace-attach-and-require rt-path ns)))
  ns)

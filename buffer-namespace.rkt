#lang racket/base
;;;
;;; Buffer Namespace
;;;

; This file contains the namespace use as the namespace for new buffers.
; The default namespace is also defined here.

;;; Provided

(provide new-buffer-namespace    ; -> namespace
         new-default-namespace)  ; -> namespace

(require "config.rkt"
         "parameters.rkt")

;;; Namespace Creation

; We need to get a reference to the modules instantiated here,
; so we can attach them the new namespace.

(define-namespace-anchor namespace-anchor)
(define this-ns (namespace-anchor->namespace namespace-anchor))

; new-buffer-namespace : -> namespace
;    Allocate a new namespace for a new buffer (see eval-buffer).
(define (new-buffer-namespace)  
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    ; Attach a module before requiring it.
    ; The module attachments are needed in order not to instantiate struct definitions multiple times.

    (namespace-attach-module this-ns 'racket/base        ns)
    (namespace-require 'racket/base)

    (namespace-attach-module this-ns "parameters.rkt"    ns)
    (namespace-require '"parameters.rkt")

    (namespace-attach-module this-ns "buffer-locals.rkt" ns)
    (namespace-require '"buffer-locals.rkt")
  ns))

; new-default-namespace : -> namespace
;    Make the (unique) default namespace.
;    This namespace holds all default values.
(define (new-default-namespace)
  (define ns (make-base-empty-namespace))
  (parameterize ([current-namespace ns])
    ; Attach module before requiring it.
    ; The module attachments are needed in order not to instantiate struct definitions multiple times.
    
    (namespace-attach-module this-ns 'racket/base        ns)
    (namespace-require 'racket/base)

    (namespace-attach-module this-ns "parameters.rkt"    ns)
    (namespace-require '"parameters.rkt")

    ; All configuration of defaults are in "config.rkt"
    ; (dynamic-require "config.rkt" #f)
    (namespace-attach-module this-ns "config.rkt"    ns)
    (namespace-require '"config.rkt")
    )
  ns)

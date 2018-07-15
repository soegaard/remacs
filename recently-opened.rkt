#lang racket/base
(provide read-recently-opened-files   ; return list of recently opened files
         write-recently-opened-files  ; write value of current-recenly-opened-files to disk
         add-recently-opened-file     ; add a single file to parameter and write disk
         reset-recently-opened-files) ; make an empty data file  
;;;
;;; Recently Opened Files
;;;

(require (for-syntax racket/base)
         racket/file
         racket/format
         racket/runtime-path
         "parameters.rkt")

(define-runtime-path .remacs-dir   ".remacs")
(define-runtime-path recently-opened.rktd ".remacs/recently.rktd")

(define no-recent-files '())

(define (read-recently-opened-files)
  (with-handlers ([exn:fail? (λ () no-recent-files)])
    (maybe-create-recently-opened-data-file)
    (define fs (file->lines recently-opened.rktd))
    (filter file-exists? fs)))

(define (write-recently-opened-files)
  (define error-msg "write-recently-opened-files: error while writing")
  (with-handlers ([exn:fail? (log-error error-msg)])
    (with-output-to-file recently-opened.rktd
      (λ ()
        (for ([f (current-recently-opened-files)]
              [_ 20])
          (displayln f)))
      #:exists 'truncate/replace)))

(define (add-recently-opened-file file)
  (log-debug (~a `(add-recently-opened-file ,file)))
  (when (path? file)
    (set! file (path->string file)))
  (define c current-recently-opened-files)
  (when (and (or (string? file) (path? file)))
    (c (cons file (c))))  
  (write-recently-opened-files)
  ((current-update-recent-files-menu)))

(define (reset-recently-opened-files)
  (parameterize ([current-recently-opened-files '()])
    (write-recently-opened-files)))

;;; Helpers

; The helpers will throw an exception on any error

(define (maybe-create-remacs-dir)
  (unless (directory-exists? .remacs-dir)
    (make-directory .remacs-dir)))

(define (maybe-create-recently-opened-data-file)
  (maybe-create-remacs-dir)
  (unless (file-exists? recently-opened.rktd)
    (with-output-to-file recently-opened.rktd
      (λ () (void)))))

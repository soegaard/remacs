#lang racket
(require "osx-keyboard.rkt")

(module+ test (require rackunit)
  ; These tests are for an US keyboard  
  ; no modifiers
  (check-equal? (key-translate kVK_ANSI_A)      "a")
  (check-equal? (key-translate kVK_ANSI_Period) ".")
  ; shift
  (check-equal? (key-translate kVK_ANSI_A      #:modifier-key-state modifier-shift-key) "A")
  (check-equal? (key-translate kVK_ANSI_Period #:modifier-key-state modifier-shift-key) ">")
  ; option
  (check-equal? (key-translate kVK_ANSI_A      #:modifier-key-state modifier-option-key) "å"))

(module+ danish-test (require rackunit)
  ; These tests are for a DK keyboard  
  ;   Use international settings to add a danish keyboard as an extra text input source.
  ;   Make sure you enable the easy access to switching (flags in the menu bar)
  
  ; no modifiers
  (check-equal? (key-translate kVK_ANSI_A)         "a")
  (check-equal? (key-translate kVK_ANSI_Period)    ".")
  (check-equal? (key-translate kVK_ANSI_Semicolon) "æ")
  ; shift
  (check-equal? (key-translate kVK_ANSI_A         #:modifier-key-state modifier-shift-key) "A")
  (check-equal? (key-translate kVK_ANSI_Semicolon #:modifier-key-state modifier-shift-key) "Æ"))

(module+ chinese-pinyin-test (require rackunit)
  ; These tests are for writing Chinese with the pinyin input method
  ;   Use international settings to add a danish keyboard as an extra text input source.
  ;   Make sure you enable the easy access to switching (flags in the menu bar)
  
  ; no modifiers
  (check-equal? (key-translate kVK_ANSI_A)         "锕")
  (check-equal? (key-translate kVK_ANSI_Period)    ".")
  (check-equal? (key-translate kVK_ANSI_Semicolon) "æ")
  ; shift
  (check-equal? (key-translate kVK_ANSI_A         #:modifier-key-state modifier-shift-key) "A")
  (check-equal? (key-translate kVK_ANSI_Semicolon #:modifier-key-state modifier-shift-key) "Æ"))

; Ignore dead keys if dead-key-state isn't provided as optional argument:
(key-translate kVK_ANSI_E #:modifier-key-state modifier-option-key) ; #\́

; A dead key should return an empty string:
(let ([dks (make-initial-dead-key-state)])
  (key-translate kVK_ANSI_E #:dead-key-state dks #:modifier-key-state modifier-option-key)) ; ""

; A dead key ´ followed by e should produce é.
(let ([dks (make-initial-dead-key-state)])
  (key-translate kVK_ANSI_E #:dead-key-state dks #:modifier-key-state modifier-option-key)
  (key-translate kVK_ANSI_E #:dead-key-state dks)) ; "é"

(let ([dks (make-initial-dead-key-state)])
  (key-translate kVK_ANSI_P #:dead-key-state dks)
  (key-translate kVK_ANSI_V #:dead-key-state dks)
  (key-translate kVK_Space  #:dead-key-state dks))

(let ([dks (make-initial-dead-key-state)])
  (list (key-translate kVK_ANSI_A #:dead-key-state dks)
        (key-translate kVK_ANSI_B #:dead-key-state dks)
        (key-translate kVK_Space  #:dead-key-state dks)))

; A dead key ˆ followed by u should produce û.
(let ([dks (make-initial-dead-key-state)])
  (list (key-translate kVK_ANSI_I #:dead-key-state dks #:modifier-key-state modifier-option-key)
        (key-translate kVK_ANSI_U #:dead-key-state dks))) ; "û"

(displayln "------")
(let ([dks (make-initial-dead-key-state)])
  (list (key-translate kVK_ANSI_A #:dead-key-state dks)
        (key-translate kVK_ANSI_B #:dead-key-state dks)
        (key-translate kVK_Space  #:dead-key-state dks)))
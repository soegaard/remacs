#lang racket
(require (for-syntax racket/base syntax/parse)
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

; The racket-lexer function returns 5 values:
; - Either a string containing the matching text or the eof object.
;   Block comments and specials currently return an empty string.
;   This may change in the future to other string or non-string data.
; - A symbol in '(error comment sexp-comment white-space constant string
;                 no-color parenthesis hash-colon-keyword symbol eof other).
; - A symbol in '(|(| |)| |[| |]| |{| |}|) or #f.
; - A number representing the starting position of the match (or #f if eof).
; - A number representing the ending position of the match (or #f if eof).

(define-lex-abbrevs
  [asterisk   "*"]
  [hyphen     "-"]  
  [underscore "_"]
  [hash       "#"]  
  [horizontal-rule  (:: (repetition 3 +inf.0 (:or hyphen asterisk underscore))
                        (:* whitespace) #\newline)]
  [header1          (:: "#" line)]
  [header2          (:: "##" line)]
  [header3          (:: "###" line)]
  [header4          (:: "####" line)]
  [header5          (:: "#####" line)]
  [header6          (:: "######" line)]
  [plain            (:: (:* (:~ (:or asterisk underscore))))]
  [bold-start       asterisk]
  [line             (:: (:* (:~ #\newline) #\newline))])

(define-syntax (START stx) (syntax/loc stx (position-offset start-pos)))
(define-syntax (END   stx) (syntax/loc stx (position-offset   end-pos)))
(define-syntax (return stx)
  (syntax-parse stx
    [(_return token-expr)
     (syntax/loc stx (values lexeme token-expr #f START END))]
    [(_return lexeme-expr token-expr)
     (syntax/loc stx (values lexeme-expr token-expr #f START END))]))

(define markdown-line-lexer
  (let ([state 0])
    (lexer
     [(eof) 'eof]
     [(special)         (return ""     'no-color)]
     [(special-comment) (return ""     'comment)]
     [horizontal-rule   (return 'horizontal-rule)]
     [header6           (return 'header6)]
     [header5           (return 'header5)]
     [header4           (return 'header4)]
     [header3           (return 'header3)]
     [header2           (return 'header2)]
     [header1           (return 'header1)]
     [plain             (return 'plain)]
     [bold-start        
      (return 'bold)]
     [line              (return 'line)])))

(define (lex-line str) (markdown-line-lexer (open-input-string str)))

(define (test-token expr expected)
  (define-values (lexeme token paren start end) (lex-line expr))
  (unless (equal? token expected)
    (error 'test-token (~a "expected " expected " got " token))))




; Horizontal Rule
;    three * - _ followed by whitespace
(test-token "***\n"   'horizontal-rule)
(test-token "***  \n" 'horizontal-rule)
(test-token "---\n"   'horizontal-rule)
(test-token "---  \n" 'horizontal-rule)
(test-token "___\n"   'horizontal-rule)
(test-token "___  \n" 'horizontal-rule)
; Headers
(test-token "# Header 1 (biggest)\n"       'header1)
(test-token "## Header 2 \n"               'header2)
(test-token "### Header 3 \n"              'header3)
(test-token "#### Header 4 \n"             'header4)
(test-token "##### Header 5 \n"            'header5)
(test-token "###### Header 6 (smallest)\n" 'header6)
; Plain Text
(test-token "plain text (unformatted)" 'plain)
; Bold
(test-token "*bold*" 'bold)
; Complicated
(test-token "foo * bar" 'line)


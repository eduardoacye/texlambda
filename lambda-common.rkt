;; 2016 - Eduardo Acuña Yeomans
;; -*- coding: utf-8; mode: racket -*-

#lang racket/base

(require racket/match)

(provide
 ;; Data types
    atom          atom?         atom-symbol
    application   application?  application-applicator  application-applicand
    abstraction   abstraction?  abstraction-argument    abstraction-body
    command       command?      command-name            command-arguments
 ;; Utility procedures
    assert!
 ;; Reader
    *terminal-delimiters* *initial-delimiters*
    terminal-delimiter?   initial-delimiter?    delimiter?
    consume-whitespace
    read-number read-identifier read-brackets read-command/atom
    read-lambda read-parentheses read-expr
    apply/identity
 ;; Evaluator
    *metacommands* *commands*
    eval-expr
 ;; Printer
    notation-abuse?
    ;; *command-printers*
    *command-formats*
    fmt:atom-pre fmt:atom-post
    fmt:command-name-pre fmt:command-name-post
    fmt:other-pre fmt:other-post
    fmt:left-paren fmt:right-paren fmt:left-brack fmt:right-brack
    fmt:application-sep fmt:abstraction-sep fmt:command-sep
    fmt:lambda fmt:dot
    format-atom format-application format-abstraction format-command format-expr
    format-list format-other
    print-expr
 )

;; DATA TYPES

(struct atom (symbol)
  ;; An atomic lambda term
  ;; (atom <symbol>)
  #:transparent)

(struct application (applicator applicand)
  ;; An application lambda term
  ;; (application [<atom>|<application>|<abstraction>|<command>]
  ;;              [<atom>|<application>|<abstraction>|<command>])
  #:transparent)

(struct abstraction (argument body)
  ;; An abstraction lambda term
  ;; (abstraction [<atom>|<command>]
  ;;              [<atom>|<application>|<abstraction>|<command>])
  #:transparent)

(struct command (name arguments)
  ;; A metalanguage command
  ;; (command <symbol>
  ;;          <list of [<atom>|<application>|<abstraction>|<command>]>)
  #:transparent)

;; UTILITY PROCEDURES
;;
;;

(define (assert! ok? who message . formats)
  ;; assert! : <any> <symbol> <string> <any>* -> [<boolean>|<error>]
  ;;
  ;; Utility procedure for checking the condition `ok?', rising an error
  ;; if the condition isn't satisfied with the information given by `who',
  ;; `message' and an optional list of objects to be formatted by the error
  ;; procedure.
  (if ok? #t (apply error who message formats)))

;; READER
;;
;; Recursive descent parser for the lambda terms language and command metalanguage

(define *terminal-delimiters*
  ;; List of the special character delimiters that end a reader state.
  '(#\} #\] #\) #\. #\, ))

(define *initial-delimiters*
  ;; List of the special character delimiters that start a reader state.
  '(#\{ #\[ #\( #\λ #\\ ))

(define (terminal-delimiter? ch)
  ;; terminal-delimiter? : [<char>|<eof>] -> <boolean>
  ;;
  ;; Return #t if `ch' is a terminal delimiter, #f otherwise.
  (or (eof-object? ch) (member ch *terminal-delimiters* char=?)))

(define (initial-delimiter? ch)
  ;; initial-delimiter? : [<char>|<eof>] -> <boolean>
  ;;
  ;; Return #t if `ch' is an initial delimiter, #f otherwise.
  (and (not (eof-object? ch)) (member ch *initial-delimiters* char=?)))

(define (delimiter? ch)
  ;; delimiter? : [<char>|<eof>] -> <boolean>
  ;;
  ;; Return #t if `ch' is a delimiter, #f otherwise.
  (or (terminal-delimiter? ch)
      (initial-delimiter? ch)
      (char-whitespace? ch)))

(define (consume-whitespace [in (current-input-port)]
                            [ch (peek-char in)])
  ;; consume-whitespace : <input-port> [<char>|<eof>] -> <null>
  ;;
  ;; Read all the whitespace characters of `in' until other characters or
  ;; <eof> is found.
  (when (and (char? ch) (char-whitespace? ch))
    (read-char in)
    (consume-whitespace in (peek-char in))))

(define (read-number [in (current-input-port)]
                     [ch (peek-char in)])
  ;; read-number : <input-port> [<char>|<eof>] -> <number>
  ;;
  ;; Read a number from `in' with a peeked character `ch'.
  (consume-whitespace in ch)
  (let loop ([lis null]
             [n   (peek-char in)])
    (cond [(and (char? n) (char-numeric? n))
           (read-char in)
           (loop (cons n lis) (peek-char in))]
          [(null? lis)
           (error 'read-number "expecting digit but got `~a'" n)]
          [else
           (assert! (delimiter? n) 'read-number
                    "missing delimiter, got `~a'" n)
           (string->number (list->string (reverse lis)))])))

(define (read-identifier [in (current-input-port)]
                         [ch (peek-char in)])
  ;; read-identifier : <input-port> [<char>|<eof>] -> <symbol>
  ;;
  ;; Read an identifier from `in' with a peeked character `ch'.
  (consume-whitespace in ch)
  (let loop ([lis null]
             [x   (peek-char in)])
    (cond [(not (delimiter? x))
           (read-char in)
           (loop (cons x lis) (peek-char in))]
          [(null? lis)
           (error 'read-identifier "expecting a non-delimiter but got `~a'" x)]
          [else
           (assert! (delimiter? x) 'read-identifier
                    "missing delimiter, got `~a'" x)
           (string->symbol (list->string (reverse lis)))])))

(define (read-brackets [in (current-input-port)]
                       [ch (peek-char in)])
  ;; read-brackets : <input-port> [<char>|<eof>] -> <list of <expr>>
  ;;
  ;; Read a list of lambda terms or commands from `in' following the syntax:
  ;; \[ [<expr> [, <expr>]*]? \]
  (consume-whitespace in ch)
  (let ([ch (read-char in)])
    (assert! (and (char? ch) (char=? ch #\[)) 'read-brackets
             "expecting `[' but got `~a'" ch))
  (let loop ([lis null]
             [arg (read-expr in)])
    (cond [(null? arg)
           (let ([ch (read-char in)])
             (cond [(eof-object? ch)
                    (error 'read-brackets "missing matching bracket")]
                   [(char=? ch #\,)
                    (loop lis (read-expr in))]
                   [(char=? ch #\])
                    (reverse lis)]
                   [else
                    (error 'read-brackets "missing matching bracket")]))]
          [else
            (loop (cons arg lis) (read-expr in))])))

(define (read-command/atom [in (current-input-port)]
                           [ch (peek-char in)])
  ;; read-command/variable : <input-port> [<char>|<eof>] -> [<command>|<atom>]
  ;;
  ;; Read an atomic lambda term or a command from `in' following the syntax:
  ;; <symbol> [\[ [<expr> [, <expr>]*]? \]]?
  (define id (read-identifier in ch))
  (consume-whitespace in)
  (let ([ch (peek-char in)])
    (cond [(and (char? ch) (char=? ch #\[))
           (command id (read-brackets in ch))]
          [else
           (atom id)])))

(define (read-lambda [in (current-input-port)]
                     [ch (peek-char in)])
  ;; read-lambda : <input-port> [<char>|<eof>] -> <abstraction>
  ;;
  ;; Read a lambda term abstraction from `in' following the syntax:
  ;; [λ|\] [<command>|<atom>]+ . <expr>+
  (consume-whitespace in ch)
  (let ([ch (read-char in)])
    (assert! (and (char? ch) (or (char=? ch #\\ ) (char=? ch #\λ))) 'read-lambda
             "expecting `λ' or `\\' but got `~a'" ch))
  (let loop ([lis (list (read-command/atom in))]
             [ch  (peek-char in)])
    (cond [(char=? ch #\.)
           (read-char in)
           (foldr abstraction (read-expr in) (reverse lis))]
          [else
           (loop (cons (read-command/atom in) lis) (peek-char in))])))


(define (apply/identity acc x)
  ;; apply/identity : <any> <any> -> [<any>|<application>]
  ;;
  ;; Return `x' if `acc' is the empty list, otherwise return the application of
  ;; `acc' to `x'.
  (if (null? acc) x (application acc x)))

(define (read-parentheses [in (current-input-port)]
                          [ch (peek-char in)])
  ;; read-parentheses : <input-port> [<char>|<eof>] -> [<application>|<abstraction>]
  ;;
  ;; Read a lambda term as if it were written between parentheses.
  (consume-whitespace in ch)
  (let ([ch (read-char in)])
    (assert! (and (char? ch) (char=? ch #\( )) 'read-parentheses
             "expecting `(' but got `~a'" ch))
  (let ([expr (read-expr in)])
    (assert! (or (application? expr) (abstraction? expr)) 'read-parentheses
             "expected an application or an abstraction but got ~a" expr)
    (let ([ch (read-char in)])
      (assert! (and (char? ch) (char=? ch #\) )) 'read-parentheses
               "missing matching parentheses"))
    expr))

(define (read-expr [in  (current-input-port)]
                   [ch  (peek-char in)]
                   [acc null])
  ;; read-expr : <input-port> [<char>|<eof>] [<nil>|<term>] -> <expr>
  ;;
  ;; Read a lambda term or a command.
  (cond [(or (eof-object? ch) (terminal-delimiter? ch))
         acc]
        [(char-whitespace? ch)
         (consume-whitespace in ch)
         (read-expr in (peek-char in) acc)]
        [(char-numeric? ch)
         (read-number in ch)]
        [(char=? ch #\( )
         (let ([expr (read-parentheses in ch)])
           (read-expr in (peek-char in) (apply/identity acc expr)))]
        [(or (char=? ch #\λ) (char=? ch #\\ ))
         (let ([expr (read-lambda in ch)])
           (read-expr in (peek-char in) (apply/identity acc expr)))]
        [else
         (let ([expr (read-command/atom in ch)])
           (read-expr in (peek-char in) (apply/identity acc expr)))]))

;; EVALUATOR
;;
;; Recursive expression evaluator

(define *metacommands*
  ;; Table associating symbols representing a metacommand name to procedures.
  (make-hasheq))

(define *commands*
  ;; Table associating symbols representing a command name to procedures.
  (make-hasheq))

(define (eval-expr expr)
  ;; eval-expr : <expr> -> <any>
  ;;
  ;; Evaluate an expression constructed with `read-expr'.
  (cond [(atom? expr)
         expr]
        [(application? expr)
         (application (eval-expr (application-applicator expr))
                      (eval-expr (application-applicand expr)))]
        [(abstraction? expr)
         (abstraction (eval-expr (abstraction-argument expr))
                      (eval-expr (abstraction-body expr)))]
        [(command? expr)
         (cond [(hash-ref *metacommands* (command-name expr) #f)
                => (lambda (proc) (apply proc (command-arguments expr)))]
               [(hash-ref *commands* (command-name expr) #f)
                => (lambda (proc) (apply proc (map eval-expr (command-arguments expr))))]
               [else
                (error 'eval-expr "unknown command ~a called with arguments ~a"
                       (command-name expr) (command-arguments expr))])]
        [else
         expr]))

;; PRINTER
;;
;; Recursive expression printer

;; Formatting atoms
(define fmt:atom-pre
  ;; Object used to format with ~a before writing the symbol.
  (make-parameter ""))

(define fmt:atom-post
  ;; Object used to format with ~a after writing the symbol.
  (make-parameter ""))

;; Formatting applications
(define fmt:application-sep
  ;; Object used to format with ~a after the applicator and before the applicand.
  (make-parameter " "))

;; Formatting abstractions
(define fmt:lambda
  ;; Object used to format the lambda with ~a.
  (make-parameter "λ"))

(define fmt:dot
  ;; Object used to format the dot with ~a.
  (make-parameter "."))

(define fmt:abstraction-sep
  ;; Object used to format with ~a between each argument while abusing the notation.
  (make-parameter " "))

;; Formatting commands
(define fmt:command-name-pre
  ;; Object used to format with ~a before writing the name.
  (make-parameter ""))

(define fmt:command-name-post
  ;; Object used to format with ~a after writing the name.
  (make-parameter ""))

(define fmt:command-sep
  ;; Object used to format with ~a between each argument.
  (make-parameter ","))

;; Formattig other
(define fmt:other-pre
  ;; Object used to format with ~a before writing the value.
  (make-parameter ""))

(define fmt:other-post
  ;; Object used to format with ~a after writing the value.
  (make-parameter ""))

;; Formatting structure
(define fmt:left-paren
  ;; Object used to format with ~a the left open parentheses.
  (make-parameter "("))

(define fmt:right-paren
  ;; Object used to format with ~a the right close parentheses.
  (make-parameter ")"))

(define fmt:left-brack
  ;; Object used to format with ~a the left open brackets.
  (make-parameter "["))

(define fmt:right-brack
  ;; Object used to format with ~a the right close brackets.
  (make-parameter "]"))

;; Format control
(define notation-abuse?
  ;; Boolean parameter that determines if the abuse of notation is turned on or off.
  (make-parameter #f))

(define *command-formats*
  ;; Table associating symbols representing a command name to a formatting procedure.
  (make-hasheq))

(define (format-atom symbol)
  ;; format-atom : <symbol> -> <string>
  (format "~a~a~a" (fmt:atom-pre) symbol (fmt:atom-post)))

(define (format-application applicator applicand)
  ;; format-application : <expr> <expr> -> <string>
  (cond
    [(notation-abuse?)
     (format "~a~a~a"
             (if (abstraction? applicator)
                 (format "~a~a~a"
                         (fmt:left-paren)
                         (format-expr applicator)
                         (fmt:right-paren))
                 (format-expr applicator))
             (fmt:application-sep)
             (if (or (application? applicand)
                     (abstraction? applicand))
                 (format "~a~a~a"
                         (fmt:left-paren)
                         (format-expr applicand)
                         (fmt:right-paren))
                 (format-expr applicand)))]
    [else
     (format "~a~a~a~a~a"
             (fmt:left-paren) (format-expr applicator)
             (fmt:application-sep)
             (format-expr applicand) (fmt:right-paren))]))

(define (format-list lis sep)
  ;; format-list : <list> <any> -> <string>
  (cond [(null? lis) ""]
        [(null? (cdr lis))
         (format-expr (car lis))]
        [else
         (format "~a~a~a"
                 (format-expr (car lis))
                 sep
                 (format-list (cdr lis) sep))]))

(define (format-abstraction argument body)
  ;; format-abstraction : [<atom>|<command>] <expr> -> <string>
  (cond [(notation-abuse?)
         (match body
           [(abstraction x M)
            (format-abstraction
             (if (list? argument) (cons x argument) (list x argument)) M)]
           [_
            (format "~a~a~a~a"
                    (fmt:lambda)
                    (if (list? argument)
                        (format-list (reverse argument) (fmt:abstraction-sep))
                        (format-expr argument))
                    (fmt:dot)
                    (format-expr body))])]
        [else
         (format "~a~a~a~a~a~a"
                 (fmt:left-paren) (fmt:lambda)
                 (format-expr argument)
                 (fmt:dot)
                 (format-expr body)
                 (fmt:right-paren))]))

(define (format-command name arguments)
  ;; format-command : <symbol> <list> -> <string>
  (cond [(hash-ref *command-formats* name #f)
         => (lambda (proc)
              (apply proc arguments))]
        [else
         (format "~a~a~a~a~a~a"
                 (fmt:command-name-pre) name (fmt:command-name-post)
                 (fmt:left-brack)
                 (format-list arguments (fmt:command-sep))
                 (fmt:right-brack))]))

(define (format-other other)
  ;; format-other : <any> -> <string>
  (format "~a~a~a"
          (fmt:other-pre)
          other
          (fmt:other-post)))

(define (format-expr expr)
  ;; format-expr : <expr> -> <string>
  (match expr
    [(atom symbol)                      (format-atom symbol)]
    [(application applicator applicand) (format-application applicator applicand)]
    [(abstraction argument body)        (format-abstraction argument body)]
    [(command name arguments)           (format-command name arguments)]
    [x                                  (format-other x)]))

(define (print-expr expr [out (current-output-port)])
  ;; print-expr : <expr> <output-port> -> <void>
  (display (format-expr expr) out))

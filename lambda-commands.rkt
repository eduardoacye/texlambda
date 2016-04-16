#lang racket/base

(require racket/match)
(require racket/list)
(require racket/set)

(require "lambda-common.rkt")

(provide
 install-procedure

 cmd:abuse
 cmd:unabuse

 fmt:fv-name
 fmt:length-pre fmt:length-post
 fmt:subst-assign
 )

(define (install-procedure table name procedure)
  (hash-set! table name procedure))

(define (cmd:quote expr)
  (match expr
    [(atom symbol)
     expr]
    [(application applicator applicand)
     (application (cmd:quote applicator)
                  (cmd:quote applicand))]
    [(abstraction argument body)
     (abstraction (cmd:quote argument)
                  (cmd:quote body))]
    [(command name arguments)
     (if (eq? name 'u)
         (cmd:unquote (car arguments))
         (command name
                  (map cmd:quote arguments)))]
    [_
     expr]))

(install-procedure *metacommands* 'q cmd:quote)

(define (cmd:unquote expr)
  (eval-expr expr))

(install-procedure *metacommands* 'u cmd:unquote)

(define (cmd:numeral expr)
  (match expr
    [(abstraction (atom symbol1) (abstraction (atom symbol2) body))
     (let loop ([body body])
       (match body
         [(== (atom symbol2))
          0]
         [(application (== (atom symbol1)) body)
          (+ 1 (loop body))]
         [_
          (error 'numeral "malformed Church encoding")]))]
    [_
     (error 'numeral "malformed Church encoding")]))

(install-procedure *commands* 'numeral cmd:numeral)

(define (cmd:length expr)
  (match expr
    [(atom symbol) 1]
    [(application applicator applicand)
     (+ (cmd:length applicator)
        (cmd:length applicand))]
    [(abstraction argument body)
     (+ 1 (cmd:length body))]
    [_
     (error 'length "can't compute the length of ~a, it's not a λ term" expr)]))

(install-procedure *commands* 'length cmd:length)

(define fmt:length-pre (make-parameter "|"))
(define fmt:length-post (make-parameter "|"))

(define (fmt:length expr)
  (format "~a~a~a"
          (fmt:length-pre)
          (format-expr expr)
          (fmt:length-post)))

(install-procedure *command-formats* 'length fmt:length)

(define (cmd:free-variables expr)
  (match expr
    [(atom symbol)
     (list expr)]
    [(application applicator applicand)
     (remove-duplicates (append (cmd:free-variables applicator)
                                (cmd:free-variables applicand)))]
    [(abstraction argument body)
     (remove argument (cmd:free-variables body))]
    [_
     (error 'length "can't compute the length of ~a, it's not a λ term" expr)]))

(install-procedure *commands* 'fv cmd:free-variables)

(define fmt:fv-name (make-parameter "FV"))

(define (fmt:free-variables expr)
  (format "~a~a~a~a"
          (fmt:fv-name)
          (fmt:left-paren)
          (format-expr expr)
          (fmt:right-paren)))

(install-procedure *command-formats* 'fv fmt:free-variables)

(define alphabetic-strings
  (map (lambda (n) (string (integer->char n)))
       (range (char->integer #\a) (char->integer #\z))))

(define (atom-from-new-symbol binding-symbols)
  (let ([diff (set-subtract (map string->symbol alphabetic-strings)
                            binding-symbols)])
    (if (not (null? diff))
        (atom (car diff))
        (let loop ([i 0])
          (let ([next-symbols (map (lambda (str)
                                     (string->symbol (string-append str (number->string i))))
                                   alphabetic-strings)])
            (let ([diff (set-subtract next-symbols binding-symbols)])
              (if (not (null? diff))
                  (atom (car diff))
                  (loop (+ i 1)))))))))

(define (subst expr old-atom new-expr binding-symbols)
  (match expr
    [(atom symbol)
     (if (equal? expr old-atom)
         new-expr
         expr)]
    [(application applicator applicand)
     (application (subst applicator old-atom new-expr binding-symbols)
                  (subst applicand old-atom new-expr binding-symbols))]
    [(abstraction argument body)
     (cond
       [(or (equal? argument old-atom)
            (not (member old-atom (cmd:free-variables body))))
        expr]
       [(not (member argument (cmd:free-variables new-expr)))
        (abstraction argument
                     (subst body old-atom new-expr
                            (set-add binding-symbols (atom-symbol argument))))]
       [else
        (let* ([binding-symbols (set-union binding-symbols
                                           (map atom-symbol (cmd:free-variables new-expr))
                                           (map atom-symbol (cmd:free-variables body)))]
               [fresh (atom-from-new-symbol binding-symbols)])
          (abstraction fresh
                       (subst (subst body argument fresh
                                     (set-add binding-symbols (atom-symbol fresh)))
                              old-atom new-expr
                              (set-add binding-symbols (atom-symbol fresh)))))])]))

(define (cmd:subst expr old-atom new-expr)
  (subst expr old-atom new-expr null))

(install-procedure *commands* 'subst cmd:subst)

(define fmt:subst-assign (make-parameter ":="))

(define (fmt:subst expr old-atom new-expr)
  (format "~a~a~a~a~a~a"
          (format-expr expr)
          (fmt:left-brack)
          (format-expr old-atom)
          (fmt:subst-assign)
          (format-expr new-expr)
          (fmt:right-brack)))

(install-procedure *command-formats* 'subst fmt:subst)

(define (cmd:abuse)
  (notation-abuse? #t)
  '(notation abuse turned on))

(install-procedure *metacommands* 'abuse cmd:abuse)

(define (cmd:unabuse)
  (notation-abuse? #f)
  '(notation abuse turned off))

(install-procedure *metacommands* 'unabuse cmd:unabuse)

(define (fmt:well-formed expr)
  (let ([state (notation-abuse?)])
    (cmd:unabuse)
    (let ([str (format-expr expr)])
      (notation-abuse? state)
      str)))

(install-procedure *command-formats* 'wf fmt:well-formed)

(define (cmd:well-formed expr)
  (cmd:quote (command 'wf (list expr))))

(install-procedure *commands* 'wf cmd:well-formed)

(define (fmt:mal-formed expr)
  (let ([state (notation-abuse?)])
    (cmd:abuse)
    (let ([str (format-expr expr)])
      (notation-abuse? state)
      str)))

(install-procedure *command-formats* 'mf fmt:mal-formed)

(define (cmd:mal-formed expr)
  (cmd:quote (command 'mf (list expr))))

(install-procedure *commands* 'mf cmd:mal-formed)

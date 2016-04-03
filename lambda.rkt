#lang racket/base
(require racket/cmdline)
(require racket/match)

(struct atm (s)   #:transparent)
(struct app (f x) #:transparent)
(struct fun (a b) #:transparent)
(struct cmd (c l) #:transparent)

;; READER

(define *terminator-delimiters* '(#\] #\) #\. #\,))
(define *accumulator-delimiters* '(#\[ #\( #\λ #\\))

(define (terminator-delimiter? c)
  (or (eof-object? c)
      (member c *terminator-delimiters* char=?)))

(define (accumulator-delimiter? c)
  (member c *accumulator-delimiters* char=?))

(define (delimiter? c)
  (or (terminator-delimiter? c)
      (accumulator-delimiter? c)
      (char-whitespace? c)))

(define (assert! ok? who msg . fmts)
  (if ok? #t (apply error who msg fmts)))

(define (read-number [in (current-input-port)] [c (peek-char in)])
  (consume-whitespace in)
  (let loop ([lis null]
             [n   c])
    (cond [(and (char? n) (char-numeric? n))
           (read-char in)
           (loop (cons n lis) (peek-char in))]
          [(null? lis)
           (error 'read-number "expecting digit but got `~a'" n)]
          [else
           (assert! (delimiter? (peek-char in))
                    'read-number "missing delimiter, got `~a'" (peek-char in))
           (string->number (list->string (reverse lis)))])))

(define (read-identifier [in (current-input-port)] [c (peek-char in)])
  (consume-whitespace in)
  (let loop ([lis null]
             [x   (peek-char in)])
    (cond [(not (delimiter? x))
           (read-char in)
           (loop (cons x lis) (peek-char in))]
          [(null? lis)
           (error 'read-identifier "expecting a non-delimiter but got `~a'" x)]
          [else
           (string->symbol (list->string (reverse lis)))])))

(define (consume-whitespace [in (current-input-port)] [c (peek-char in)])
  (when (and (char? c) (char-whitespace? c))
    (read-char in)
    (consume-whitespace in (peek-char in))))

(define (read-lambda [in (current-input-port)] [c (peek-char in)])
  (assert! (or (char=? c #\λ) (char=? c #\\))
           'read-lambda "expecting `λ' but got `~a'" c)
  (read-char in)
  (let loop ([lis (list (read-command/variable in))])
    (let ([c (peek-char in)])
      (cond [(char=? c #\.)
             (read-char in)
             (foldr fun (read-expr in) (reverse lis))]
            [else
             (loop (cons (read-command/variable in) lis))]))))

(define (apply/id acc x)
  (if (null? acc) x (app acc x)))

(define (read-parentheses [in (current-input-port)] [c (peek-char in)])
  (assert! (char=? c #\()
           'read-parentheses "expecting `(' but got `~a'" c)
  (read-char in)
  (let ([expr (read-expr in)])
    (assert! (or (app? expr) (fun? expr))
             'read-parentheses "expected an application or a function but got ~a" expr)
    (assert! (char=? (read-char in) #\))
             'read-parentheses "missing matching parentheses")
    expr))

(define (read-brackets [in (current-input-port)] [c (peek-char in)])
  (assert! (char=? c #\[)
           'read-brackets "expecting `[' but got `~a'" c)
  (read-char in)
  (let loop ([lis null])
    (let ([arg (read-expr in)])
      (cond [(null? arg)
             (let ([c (read-char in)])
               (cond [(eof-object? c)
                      (error 'read-brackets "missing matching bracket")]
                     [(char=? c #\,)
                      (loop lis)]
                     [(char=? c #\])
                      (reverse lis)]
                     [else
                      (error 'read-brackets "missing matching bracket")]))]
            [else
             (loop (cons arg lis))]))))

(define (read-command/variable [in (current-input-port)] [c (peek-char in)])
  (define id (read-identifier in c))
  (consume-whitespace in)
  (let ([c (peek-char in)])
    (cond [(and (char? c) (char=? c #\[))
           (define args (read-brackets in c))
           (cmd id args)]
          [else
           (atm id)])))

(define (read-expr [in (current-input-port)] [acc null] [c (peek-char in)])
  (cond [(or (eof-object? c) (terminator-delimiter? c))
         acc]
        [(char-whitespace? c)
         (consume-whitespace in c)
         (read-expr in acc)]
        [(char-numeric? c)
         (read-number in c)]
        [(char=? c #\()
         (read-expr in (apply/id acc (read-parentheses in c)))]
        [(or (char=? c #\λ) (char=? c #\\))
         (read-expr in (apply/id acc (read-lambda in c)))]
        [else
         (read-expr in (apply/id acc (read-command/variable in c)))]))

;; EVALUATOR

(define (eval-expr expr)
  (cond [(atm? expr)
         expr]
        [(app? expr)
         (app (eval-expr (app-f expr)) (eval-expr (app-x expr)))]
        [(fun? expr)
         (fun (eval-expr (fun-a expr)) (eval-expr (fun-b expr)))]
        [(cmd? expr)
         (cond [(hash-ref *meta* (cmd-c expr) #f)
                => (lambda (proc) (apply proc (cmd-l expr)))]
               [(hash-ref *commands* (cmd-c expr) #f)
                => (lambda (proc) (apply proc (map eval-expr (cmd-l expr))))])]
        [else
         expr]))

;; RUNTIME

(define (cmd:numeral term)
  (match term
    [(fun (atm f) (fun (atm x) body))
     (let loop ([body body])
       (match body
         [(atm x) 0]
         [(app (atm f) x) (+ 1 (loop x))]
         [_
          (error 'numeral "Malformed Church encoding, see numeral[help]")]))]
    [(atm 'help)
     (display (format "Church encoding for a number `n' is =α λf x.f(f(... (f x))) with n f's"))]
    [_
     (error 'numeral "Malformed Church encoding, see numeral[help]")]))

(define (cmd:quote term)
  term)

(define *commands*
  (hasheq
   'atm atm
   'app app
   'fun fun
   'cmd cmd
   'numeral cmd:numeral
   'exit exit))

(define *meta*
  (hasheq
   'quote cmd:quote))

;; READ EVAL PRINT LOOP

(define (repl)
  (parameterize ([current-prompt-read
                  (lambda ()
                    (display "λ> ")
                    (flush-output)
                    (let ([str (read-line)])
                      (if (or (eof-object? str) (string=? str "!!!"))
                          eof
                          (read-expr (open-input-string str)))))]
                 [current-eval
                  (lambda (expr) (eval-expr (cdr expr)))])
    (read-line)
    (read-eval-print-loop)))

;; TEXLAMBDA

(define (texlambda entry)
  ;; TODO
  '())

;; COMMAND LINE INTERFACE

(define cmdln-repl? (make-parameter #f))
(define cmdln-file? (make-parameter #f))
(define cmdln-filename (make-parameter #f))
(define cmdln-expr? (make-parameter #f))
(define cmdln-expr (make-parameter #f))
(define cmdln-latex? (make-parameter #f))
(define cmdln-latex-entry (make-parameter #f))

(define console-args
  (command-line
   #:program "lambda"
   #:once-any
   [("-r" "--repl")          "Run a read-eval-print-loop"
    (cmdln-repl? #t)]
   [("-f" "--file") filename "Evaluate a file"
    (cmdln-file? #t)
    (cmdln-filename filename)]
   [("-e" "--expr") expr "Evaluate an expression"
    (cmdln-expr? #t)
    (cmdln-expr expr)]
   [("-l" "--latex") entry "Run latex with texlambda"
    (cmdln-latex? #t)
    (cmdln-latex-entry entry)]))

(when (cmdln-repl?)
  (repl))

(when (cmdln-file?)
  (call-with-input-file (cmdln-filename)
    (lambda (in)
      (eval-expr (read-expr in)))))

(when (cmdln-expr?)
  (eval-expr (read-expr (open-input-string (cmdln-expr)))))

(when (cmdln-latex?)
  (texlambda (cmdln-latex-entry)))

#lang racket/base
(require racket/cmdline)
(require racket/match)
(require racket/date)
(require racket/set)
(require racket/system)
(require racket/file)

(struct atm (s)   #:transparent)
(struct app (f x) #:transparent)
(struct fun (a b) #:transparent)
(struct cmd (c l) #:transparent)

;; READER

(define *terminator-delimiters* '(#\} #\] #\) #\. #\,))
(define *accumulator-delimiters* '(#\{ #\[ #\( #\λ #\\))

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

;; TEXLAMBDA

(define (format-tex-atm s)
  (format "~a" s))

(define (format-tex-fun a b)
  (format "(\\lambda\\ ~a\\ .\\ ~a)" (format-tex a) (format-tex b)))

(define (format-tex-app f x)
  (format "(~a\\ ~a)" (format-tex f) (format-tex x)))

(define (format-tex-cmd c l)
  (format "\\texttt{~a[} ~a \\texttt{]}" c (format-tex-list l ",")))

(define (format-tex-list l sep)
  (cond [(null? l) ""]
        [(null? (cdr l)) (format "~a" (format-tex (car l)))]
        [else (format "~a~a~a" (format-tex (car l)) sep (format-tex-list (cdr l) sep))]))

(define (format-tex term)
  (match term
    [(atm s)   (format-tex-atm s)]
    [(fun a b) (format-tex-fun a b)]
    [(app f x) (format-tex-app f x)]
    [(cmd c l)
     (match c
       ['tex* (format-tex* (car l))]
       [_ (format-tex-cmd c l)])]
    [x         (format "\\texttt{~a}" x)]))

(define (format-tex-list* l sep)
  (cond [(null? l) ""]
        [(null? (cdr l)) (format "~a" (format-tex* (car l)))]
        [else (format "~a~a~a" (format-tex* (car l)) sep (format-tex-list* (cdr l) sep))]))

(define (format-tex-fun* arg body)
  (match body
    [(fun a b) (format-tex-fun* (if (list? arg) (cons a arg) (list a arg)) b)]
    [_ (format "\\lambda\\ ~a\\ .\\ ~a"
               (if (list? arg) (format-tex-list* (reverse arg) "\\ ") (format-tex* arg))
               (format-tex* body))]))

(define (format-tex-app* M N)
  (match (app M N)
    [(app (atm x) (atm y))     (format "~a\\ ~a"     (format-tex* M) (format-tex* N))]
    [(app (atm x) (app P Q))   (format "~a\\ (~a)"   (format-tex* M) (format-tex* N))]
    [(app (atm x) (fun y P))   (format "~a\\ (~a)"   (format-tex* M) (format-tex* N))]
    [(app (atm x) y)           (format "(~a\\ ~a)"   (format-tex* M) (format-tex* N))]
    [(app (app P Q) (atm y))   (format "~a\\ ~a"     (format-tex* M) (format-tex* N))]
    [(app (app P Q) (app R S)) (format "~a\\ (~a)"   (format-tex* M) (format-tex* N))]
    [(app (app P Q) (fun y R)) (format "~a\\ (~a)"   (format-tex* M) (format-tex* N))]
    [(app (app P Q) y)         (format "(~a\\ ~a)"   (format-tex* M) (format-tex* N))]
    [(app (fun x P) (atm y))   (format "(~a)\\ ~a"   (format-tex* M) (format-tex* N))]
    [(app (fun x P) (app R S)) (format "(~a)\\ (~a)" (format-tex* M) (format-tex* N))]
    [(app (fun x P) (fun y R)) (format "(~a)\\ (~a)" (format-tex* M) (format-tex* N))]
    [(app (fun x P) y)         (format "(~a\\ ~a)"   (format-tex* M) (format-tex* N))]))

(define (format-tex* term)
  (match term
    [(atm s)   (format-tex-atm s)]
    [(fun a b) (format-tex-fun* a b)]
    [(app f x) (format-tex-app* f x)]
    [(cmd c l)
     (match c
       ['tex (format-tex (car l))]
       [_    (format-tex-cmd c l)])]
    [x         (format "\\texttt{~a}" x)]))

(define (tex-process-file filename)
  (call-with-input-file filename
    (lambda (in)
      (call-with-output-file (string-append "./lambda-cache/" filename)
        (lambda (out)
          (let loop ([braces? #f])
            (cond
              [(eof-object? (peek-char in)) #t]
              [(string=? "\\lc{" (peek-string 4 0 in))
               (read-string 4 in)
               (let ([term (eval-expr (read-expr in))])
                 (display (format-tex term) out))
               (loop #t)]
              [(string=? "\\lc*{" (peek-string 5 0 in))
               (read-string 5 in)
               (let ([term (eval-expr (read-expr in))])
                 (display (format-tex* term) out))
               (loop #t)]
              [(char=? (peek-char in) #\})
               (if braces? (read-char in) (write-char (read-char in) out))
               (loop #f)]
              [(string=? "\\input{" (peek-string 7 0 in))
               (write-string (read-string 7 in) out)
               (let loop ([c   (peek-char in)]
                          [lis null])
                 (if (char=? c #\})
                     (let ([ref (list->string (reverse lis))])
                       (let ([ref-filename (string-append ref ".tex")])
                         (unless (file-exists? (string-append "./lambda-cache/" ref-filename))
                           (tex-process-file ref-filename))))
                     (begin
                       (write-char (read-char in) out)
                       (loop (peek-char in) (cons c lis)))))
               (loop braces?)]
              [(string=? "\\bibliography{" (peek-string 14 0 in))
               (write-string (read-string 14 in) out)
               (let loop ([c   (peek-char in)]
                          [lis null])
                 (if (char=? c #\})
                     (let ([bib (list->string (reverse lis))])
                       (unless (file-exists? (string-append "./lambda-cache/" bib ".bib"))
                         (copy-file (string-append bib ".bib")
                                    (string-append "./lambda-cache/" bib ".bib"))))
                     (begin
                       (write-char (read-char in) out)
                       (loop (peek-char in) (cons c lis)))))
               (loop braces?)]
              [else
               (write-char (read-char in) out)
               (loop braces?)])))))))

(define (texlambda entry)
  ;; TODO
  (when (directory-exists? "./lambda-cache")
    (delete-directory/files	"./lambda-cache"))
  (make-directory "./lambda-cache")
  (let ([entry-tex (string-append entry ".tex")])
    (if (file-exists? entry-tex)
        (tex-process-file entry-tex)
        (error 'texlambda "file ~a doesn't exist" entry-tex))
    (current-directory "./lambda-cache")
    (system (format "pdflatex -draftmode -interaction=batchmode ~a >/dev/null" entry))
    (system (format "bibtex ~a >/dev/null" entry))
    (system (format "pdflatex -draftmode -interaction=batchmode ~a >/dev/null" entry))
    (system (format "pdflatex -interaction=batchmode -shell-escape ~a >/dev/null" entry))))


;; RUNTIME

(define (cmd:numeral term)
  (match term
    [(fun (atm f) (fun (atm x) body))
     (let loop ([body body])
       (match body
         [(== (atm x)) 0]
         [(app (== (atm f)) body) (+ 1 (loop body))]
         [_
          (error 'numeral "Malformed Church encoding, see numeral[help]")]))]
    [(atm 'help)
     (display
      (format ";Church encoding for a number `n' is =α λf x.f(f(... (f x))) with n f's\n"))]
    [_
     (error 'numeral "Malformed Church encoding, see numeral[help]")]))

(define (cmd:quote expr)
  (match expr
    [(atm s) expr]
    [(app f x) (app (cmd:quote f) (cmd:quote x))]
    [(fun a b) (fun (cmd:quote a) (cmd:quote b))]
    [(cmd c l)
     (match c
       ['u (eval-expr (car l))]
       [_  (cmd c (map cmd:quote l))])]
    [_
     expr]))

(define (cmd:unquote expr)
  (eval-expr expr))

(define *commands*
  (hasheq
   'atm atm
   'app app
   'fun fun
   'cmd cmd
   'numeral cmd:numeral
   'tex format-tex
   'tex* format-tex*
   'exit exit))

(define *meta*
  (hasheq
   'q cmd:quote
   'u cmd:unquote))

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
    (display "REPL ready, press any key to start...")
    (flush-output)
    (read-line)
    (read-eval-print-loop)))

;; COMMAND LINE INTERFACE

(define cmdln-repl?       (make-parameter #f))
(define cmdln-file?       (make-parameter #f))
(define cmdln-filename    (make-parameter #f))
(define cmdln-expr?       (make-parameter #f))
(define cmdln-expr        (make-parameter #f))
(define cmdln-latex?      (make-parameter #f))
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

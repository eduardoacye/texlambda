#lang racket

(define (parse str)
  (read
   (open-input-string
    (string-append
     "("
     (regexp-replace* #rx"\\." (regexp-replace* #rx"\\\\" str " lambda ( ") " ) ")
     ")"))))

(define (normalize x)
  (match x
    [(list a) (normalize a)]
    [(list 'lambda (list args ...) body ...)
     (list 'lambda
           (list (car args))
           (normalize (if (null? (cdr args)) body (list 'lambda (cdr args) body))))]
    [(list a b ...)
     (foldl (lambda (x y) (list y x))
            (normalize a)
            (map normalize b))]
    [(? symbol? a) a]))

(define (denormalize x)
  (match x
    [(? symbol? a) a]
    [(list 'lambda (list arg) body)
     (cons 'lambda (cons (uncurry-args x) (denormalize (uncurry-body x))))]
    [(list a b)
     (match a
       [(? symbol? x)
        (list (denormalize a) (denormalize b))]
       [(list 'lambda (list arg) body)
        (list (denormalize a) (denormalize b))]
       [_
        (append (denormalize a) (list (denormalize b)))])]
    [(list a) x]))

(define (uncurry-args x)
  (match x
    [(list 'lambda (list arg) body) (cons arg (uncurry-args body))]
    [_ null]))

(define (uncurry-body x)
  (match x
    [(list 'lambda (list arg) body) (uncurry-body body)]
    [(? symbol? a) (list a)]
    [_ x]))

(define (read-lambda x)
  (normalize (parse x)))

(define (latexify x)
  (match x
    [(? symbol? a)
     (format "~a~a~a" (lc:variable-pre) a (lc:variable-post))]
    [(list 'lambda (list arg) body)
     (format "~a~a~a~a~a~a"
             (lc:left-paren)
             (lc:lambda)
             (latexify arg)
             (lc:abstraction-separator)
             (latexify body)
             (lc:right-paren))]
    [(list a b)
     (format "~a~a~a~a~a"
             (lc:left-paren)
             (latexify a)
             (lc:application-separator)
             (latexify b)
             (lc:right-paren))]))

(define (latexify* x)
  (match x
    [(? symbol? a)
     (format "~a~a~a"
             (lc:variable-pre)
             a
             (lc:variable-post))]
    [(list 'lambda (list args ...) body ...)
     (format "~a~a~a~a"
             (lc:lambda)
             (string-join (map latexify* args) (lc:application-separator))
             (lc:abstraction-separator)
             (latexify* body))]
    [(list a (list b ...) c ...)
     (match a
       [(list 'lambda (list args ...) body ...)
        (format "~a~a~a~a~a~a~a~a~a"
                (lc:left-paren)
                (latexify* a)
                (lc:right-paren)
                (lc:application-separator)
                (lc:left-paren)
                (latexify* b)
                (lc:right-paren)
                (if (null? c) "" (lc:application-separator))
                (latexify* c))]
       [_
        (format "~a~a~a~a~a~a~a"
                (latexify* a)
                (lc:application-separator)
                (lc:left-paren)
                (latexify* b)
                (lc:right-paren)
                (if (null? c) "" (lc:application-separator))
                (latexify* c))])]
    [(list a b ...)
     (match a
       [(list 'lambda (list args ...) body ...)
        (format "~a~a~a~a~a"
                (lc:left-paren)
                (latexify* a)
                (lc:right-paren)
                (if (null? b) "" (lc:application-separator))
                (latexify* b))]
       [_
        (format "~a~a~a"
                (latexify* a)
                (if (null? b) "" (lc:application-separator))
                (latexify* b))])]
    ['() ""]))

(define (assert-predicate pred)
  (lambda (x)
    (if (pred x)
        x
        (error "Expected argument doesn't satisfy predicate" x pred))))

(define lc:left-paren            (make-parameter "\\left( "   (assert-predicate string?)))
(define lc:right-paren           (make-parameter " \\right)"  (assert-predicate string?)))
(define lc:abstraction-separator (make-parameter " . "        (assert-predicate string?)))
(define lc:lambda                (make-parameter " \\lambda " (assert-predicate string?)))
(define lc:variable-pre          (make-parameter ""           (assert-predicate string?)))
(define lc:variable-post         (make-parameter ""           (assert-predicate string?)))
(define lc:application-separator (make-parameter "\\ "        (assert-predicate string?)))

(define spaced?         (make-parameter #f (assert-predicate boolean?)))
(define bold-variables? (make-parameter #f (assert-predicate boolean?)))
(define bold-lambdas?   (make-parameter #f (assert-predicate boolean?)))
(define bold-dots?      (make-parameter #f (assert-predicate boolean?)))
(define explicit?       (make-parameter #f (assert-predicate boolean?)))
(define term            (make-parameter "" (assert-predicate string?)))

(define console-args
  (command-line
   #:program "TeX-LaMbDa"
   #:once-each
   [("-s" "--spaced")          "Spaced terms mode - Introduces spacing"         (spaced? #t)]
   [("-v" "--bold-variables") "Bold variables mode - Make variable names bold" (bold-variables? #t)]
   [("-l" "--bold-lambdas")   "Bold lambdas mode - Makes lambdas bold"         (bold-lambdas? #t)]
   [("-d" "--bold-dots")      "Bold dots mode - Makes dots bold"               (bold-dots? #t)]
   [("-e" "--explicit")        "Explicit mode - Removes abuse of notation"      (explicit? #t)]
   #:args (str)
   str))

(term console-args)

(when (bold-variables?)
  (lc:variable-pre "\\boldsymbol{")
  (lc:variable-post "}"))

(when (bold-lambdas?)
  (lc:lambda " \\boldsymbol{\\lambda} "))

(when (bold-dots?)
  (lc:abstraction-separator " \\boldsymbol{.} "))

(when (spaced?)
  (lc:left-paren "\\left(\\ ")
  (lc:right-paren "\\ \\right)")
  (lc:lambda (string-append (lc:lambda) "\\ "))
  (lc:abstraction-separator (string-append "\\ " (lc:abstraction-separator) "\\ ")))

(if (explicit?)
    (display (latexify (normalize (parse (term)))))
    (display (latexify* (denormalize (normalize (parse (term)))))))
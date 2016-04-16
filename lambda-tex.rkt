#lang racket/base

(require racket/system)
(require racket/file)
(require racket/cmdline)
(require racket/list)

(require "lambda-common.rkt")
(require "lambda-commands.rkt")

(fmt:left-paren "\\left( ")
(fmt:right-paren " \\right)")
(fmt:left-brack "\\left[ ")
(fmt:right-brack " \\right]")
(fmt:command-name-pre "\\texttt{")
(fmt:command-name-post "}")
(fmt:application-sep "\\ ")
(fmt:abstraction-sep "\\ ")
(fmt:command-sep "\\texttt{,}")
(fmt:lambda " \\lambda ")
(fmt:dot ".")
(fmt:fv-name "\\mathrm{FV}")
(fmt:length-pre "\\left\\|")
(fmt:length-post "\\right\\|")

(define (fmt:prime expr n)
  (format "~a^{~a}"
          (format-expr expr)
          (make-string n #\')))

(install-procedure *command-formats* 'prime fmt:prime)

(define (fmt:subscript expr sub)
  (format "~a_{~a}"
          (format-expr expr)
          (format-expr sub)))

(install-procedure *command-formats* 'subscript fmt:subscript)

(define (fmt:superscript expr sup)
  (format "~a^{~a}"
          (format-expr expr)
          (format-expr sup)))

(install-procedure *command-formats* 'superscript fmt:superscript)

(define (fmt:seq expr)
  (format "\\vec{~a}"
          (format-expr expr)))

(install-procedure *command-formats* 'seq fmt:seq)

(define (fmt:dots pos . exprs)
  (let ([head (take exprs pos)]
        [tail (drop exprs pos)])
    (format-list (append head (list "...") tail) ",")))

(install-procedure *command-formats* 'dots fmt:dots)

(define (fmt:subterm expr1 expr2)
  (format "~a \\subset ~a"
          (format-expr expr1)
          (format-expr expr2)))

(install-procedure *command-formats* 'subterm fmt:subterm)

(define (fmt:subterms expr)
  (format "\\mathrm{Sub}\\left( ~a \\right)"
          (format-expr expr)))

(install-procedure *command-formats* 'subterms fmt:subterms)

(define (fmt:left-apply sup expr1 expr2)
  (format-application
   (command 'superscript (list expr1 sup))
   expr2))

(install-procedure *command-formats* 'left-apply fmt:left-apply)

(define (fmt:right-apply sup expr1 expr2)
  (format-application
   expr1
   (command 'superscript (list expr2 (format "\\sim ~a" (format-expr sup))))))

(install-procedure *command-formats* 'right-apply fmt:right-apply)

(define cache-dir (make-parameter "./lambda-cache/"))

(define (file-extension name ext)
  (string-append name ext))

(define (cache-path filename)
  (string-append (cache-dir) filename))

(define (tex-before-curly in out)
  (let loop [(c   (peek-char in))
             (lis null)]
    (if (char=? c #\})
        (list->string (reverse lis))
        (begin
          (write-char (read-char in) out)
          (loop (peek-char in) (cons c lis))))))

(define (tex-process-file filename)
  (call-with-input-file filename
    (lambda (in)
      (call-with-output-file (cache-path filename)
        (lambda (out)
          (let loop ([braces? #f])
            (cond
              [(eof-object? (peek-char in)) #t]
              [(string=? "\\lc{" (peek-string 4 0 in))
               (read-string 4 in)
               (print-expr (eval-expr (read-expr in)) out)
               (loop #t)]
              [(string=? "\\lc*{" (peek-string 5 0 in))
               (read-string 5 in)
               (cmd:abuse)
               (print-expr (eval-expr (read-expr in)) out)
               (cmd:unabuse)
               (loop #t)]
              [(char=? (peek-char in) #\})
               (if braces? (read-char in) (write-char (read-char in) out))
               (loop #f)]
              [(string=? "\\input{" (peek-string 7 0 in))
               (write-string (read-string 7 in) out)
               (let ([ref (tex-before-curly in out)])
                 (unless (file-exists? (string-append "./lambda-cache/" ref ".tex"))
                   (tex-process-file (string-append ref ".tex"))))
               (loop braces?)]
              [(string=? "\\bibliography{" (peek-string 14 0 in))
               (write-string (read-string 14 in) out)
               (let ([bib (tex-before-curly in out)])
                 (unless (file-exists? (string-append "./lambda-cache/" bib ".bib"))
                   (copy-file (string-append bib ".bib")
                              (string-append "./lambda-cache/" bib ".bib"))))
               (loop braces?)]
              [else
               (write-char (read-char in) out)
               (loop braces?)])))))))

(define (build-tex-draft entry)
  (system (format "pdflatex -draftmode -interaction=batchmode ~a >/dev/null" entry)))

(define (build-tex-bib entry)
  (system (format "bibtex ~a >/dev/null" entry)))

(define (build-tex-complete entry)
  (system (format "pdflatex -interaction=batchmode -shell-escape ~a >/dev/null" entry)))

(define (build-tex-document entry)
  (current-directory (cache-dir))
  (build-tex-draft entry)
  (build-tex-bib entry)
  (build-tex-draft entry)
  (build-tex-complete entry))

(define (texlambda entry)
  (when (directory-exists? (cache-dir))
    (delete-directory/files (cache-dir)))
  (make-directory (cache-dir))
  (tex-process-file (file-extension entry ".tex"))
  (build-tex-document entry))

(define console-args
  (command-line
   #:program "texlambda"
   #:args (entry)
   entry))

(texlambda console-args)

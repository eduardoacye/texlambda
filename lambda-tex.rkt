;; 2016 - Eduardo Acuña Yeomans
;; -*- coding: utf-8; mode: racket -*-

#lang racket/base

(require racket/system)
(require racket/file)
(require racket/cmdline)
(require racket/list)

(require "lambda-common.rkt")
(require "lambda-commands.rkt")

;; FORMATING FOR LaTeX MATH MODE

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
  ;; fmt:prime : <expr> <number> -> <string>
  ;;
  ;; expr^{\prime ... \prime} (\prime n times)
  ;;
  ;; examples:
  ;; (fmt:prime (atom 'M) 2) => $M^{\prime \prime}$
  ;; (fmt:prime (atom 'M) 4) => $M^{\prime \prime \prime \prime}$
  (format "~a^{~a}"
          (format-expr expr)
          (make-string n #\')))

(install-procedure *command-formats* 'prime fmt:prime)

(define (fmt:subscript expr sub)
  ;; fmt:subscript : <expr> <expr> -> <string>
  ;;
  ;; expr_{sub}
  ;;
  ;; examples:
  ;; (fmt:subscript (atom 'M) 2) => $M_{2}$
  ;; (fmt:subscript (atom 'M) (abstraction (atom 'x) (atom 'x))) => $M_{(\lambda x.x)}$
  (format "~a_{~a}"
          (format-expr expr)
          (format-expr sub)))

(install-procedure *command-formats* 'subscript fmt:subscript)

(define (fmt:superscript expr sup)
  ;; fmt:superscript : <expr> <expr> -> <string>
  ;;
  ;; expr^{sup}
  ;;
  ;; examples:
  ;; (fmt:superscript (atom 'M) 4) => $M^{4}$
  ;; (fmt:superscript (atom 'M) (abstraction (atom 'x) (atom 'x))) => $M^{(\lambda x.x)}$
  (format "~a^{~a}"
          (format-expr expr)
          (format-expr sup)))

(install-procedure *command-formats* 'superscript fmt:superscript)

(define (fmt:seq expr)
  ;; fmt:seq : <expr> -> <string>
  ;;
  ;; \vec{expr}
  ;;
  ;; examples:
  ;; (fmt:seq (atom 'x)) => $\vec{x}$
  (format "\\vec{~a}"
          (format-expr expr)))

(install-procedure *command-formats* 'seq fmt:seq)

(define (fmt:dots pos . exprs)
  ;; fmt:dots : <number> <expr>* -> <string>
  ;;
  ;; expr[1-pos] ... exprs[pos+1-n]
  ;;
  ;; examples:
  ;; (fmt:dots 2 (atom 'a) (atom 'b) (atom 'z)) => $a,\ b,\ ...,\ z$
  ;; (fmt:dots 1 (atom 'a) (atom 'y) (atom 'z)) => $a,\ ...,\ y,\ z$
  (let ([head (take exprs pos)]
        [tail (drop exprs pos)])
    (format-list (append head (list "...") tail) ",")))

(install-procedure *command-formats* 'dots fmt:dots)

(define (fmt:subterm expr1 expr2)
  ;; fmt:subterm : <expr> <expr> -> <string>
  ;;
  ;; expr1 $\subset$ expr2
  (format "~a \\subset ~a"
          (format-expr expr1)
          (format-expr expr2)))

(install-procedure *command-formats* 'subterm fmt:subterm)

(define (fmt:subterms expr)
  ;; fmt:subterms : <expr> -> <string>
  ;;
  ;; $\mathrm{Sub}(\texttt{expr})$
  (format "\\mathrm{Sub}\\left( ~a \\right)"
          (format-expr expr)))

(install-procedure *command-formats* 'subterms fmt:subterms)

(define (fmt:left-apply sup expr1 expr2)
  ;; fmt:left-apply : <number> <expr> <expr> -> <string>
  ;;
  ;; (expr1^{sup} expr2)
  ;;
  ;; examples:
  ;; (fmt:left-apply 3 (atom 'F) (atom 'M)) => $(F^{3}\ M)$
  (format-application
   (command 'superscript (list expr1 sup))
   expr2))

(install-procedure *command-formats* 'left-apply fmt:left-apply)

(define (fmt:right-apply sup expr1 expr2)
  ;; fmt:right-apply : <number> <expr> <expr> -> <string>
  ;;
  ;; (expr1 expr2^{~ sup})
  ;;
  ;; examples:
  ;; (fmt:right-apply 3 (atom 'F) (atom 'M)) => $(F\ M^{\sim 3})$
  (format-application
   expr1
   (command 'superscript (list expr2 (format "\\sim ~a" (format-expr sup))))))

(install-procedure *command-formats* 'right-apply fmt:right-apply)

;; LaTeX FILE PROCESSING
;;
;;

(define cache-dir
  ;; String representing the directory where the processed files will be stored.
  (make-parameter "./lambda-cache/"))

(define (file-extension name ext)
  ;; file-extension : <string> <string> -> <string>
  (string-append name ext))

(define (cache-path filename)
  ;; cache-path : <string> <string>
  (string-append (cache-dir) filename))

(define (tex-before-curly in out)
  ;; tex-before-curly : <input-port> <output-port> -> <string>
  (let loop [(c   (peek-char in))
             (lis null)]
    (if (char=? c #\})
        (list->string (reverse lis))
        (begin
          (write-char (read-char in) out)
          (loop (peek-char in) (cons c lis))))))

(define (tex-process-file filename)
  ;; tex-process-file : <string> -> #t
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
  ;; build-tex-draft : <string> -> <boolean>
  (system (format "pdflatex -draftmode -interaction=batchmode ~a >/dev/null" entry)))

(define (build-tex-bib entry)
  ;; build-tex-bib : <string> -> <boolean>
  (system (format "bibtex ~a >/dev/null" entry)))

(define (build-tex-complete entry)
  ;; build-tex-complete : <string> -> <boolean>
  (system (format "pdflatex -interaction=batchmode -shell-escape ~a >/dev/null" entry)))

(define (build-tex-document entry)
  ;; build-tex-document : <string> -> <boolean>
  (current-directory (cache-dir))
  (build-tex-draft entry)
  (build-tex-bib entry)
  (build-tex-draft entry)
  (build-tex-complete entry))

(define (texlambda entry)
  ;; texlambda : <string> -> <boolean>
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

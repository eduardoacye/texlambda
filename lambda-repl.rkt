;; 2016 - Eduardo Acuña Yeomans
;; -*- coding: utf-8; mode: racket -*-

#lang racket/base

(require racket/cmdline)

(require "lambda-common.rkt")
(require "lambda-commands.rkt")

(define (repl)
  ;; repl : <void> -> <void>
  ;; 
  ;; Run a Read Eval Print Loop.
  (parameterize ([current-prompt-read
                  (lambda ()
                    (display "λ> ")
                    (flush-output)
                    (let ([str (read-line)])
                      (if (or (eof-object? str) (string=? str "exit"))
                          eof
                          (read-expr (open-input-string str)))))]
                 [current-eval
                  (lambda (expr)
                    (eval-expr (cdr expr)))]
                 [current-print
                  (lambda (expr)
                    (print-expr expr)
                    (newline))])
    (display "REPL ready, press any key to start...")
    (flush-output)
    (read-line)
    (read-eval-print-loop)))

(define console-args
  (command-line
   #:program "texlambda"))

(repl)

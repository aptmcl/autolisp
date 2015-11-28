#lang racket/base

#|

This provides AutoLisp scope, AutoLisp arithmetic, AutoLisp booleans
and AutoLisp lists

|#

(require "asaaabal-pre.rkt")
(provide (all-from-out "asaaabal-pre.rkt"))

;; (define asaaabal-namespace (make-base-empty-namespace))

;; (parameterize ((current-namespace asaaabal-namespace))
;;   (namespace-require/constant "asaaabal-pre.ss"))

(define (al:eval expr)
  (eval expr (current-namespace)))

(define (al:apply function list)
  (if (or (symbol? function)
          (pair? function))
      (apply (al:eval function) list)
      (error "bad function" function)))

(define (al:mapcar func . lsts)
  (let ((func (al:eval func)))
    (if (member 'nil lsts)
        'nil
        (let mapit ((lsts lsts))
          (if (ormap null? lsts)
              (list)
              (cons (apply func (map car lsts))
                    (mapit (map cdr lsts))))))))


(let ()
  (SETQ EVAL al:eval)
  (SETQ APPLY al:apply)
  (SETQ MAPCAR al:mapcar)
  (void))


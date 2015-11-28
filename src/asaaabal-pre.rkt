#lang racket/base
(require (for-syntax racket/base))
(require racket/port)
(require rackunit)

#|

This provides AutoLisp scope, AutoLisp arithmetic, AutoLisp booleans
and AutoLisp lists

|#

(require "support.rkt")
(require (for-syntax "support.rkt"))
(require "../lang/readtable.rkt")
(require (for-syntax racket/list))

(define-syntax using-input
  (syntax-rules ()
    ((_ str expr)
     (with-input-from-string str
       (lambda ()
         expr)))))

(define-syntax check-equal-output?
  (syntax-rules ()
    ((_ expr expected-output expected-result)
     (let ((res #f))
       (check-equal? 
        (with-output-to-string
          (lambda ()
            (set! res expr)))
        expected-output)
       (check-equal? res expected-result)))))

(define epsilon 0.00001)

(define-syntax (check stx)
  (syntax-case stx ()
    ((_ expr val)
     (if (inexact-real? (syntax->datum #'val))
         #'(let ((exp expr))
             (if (number? exp)
                 (check-= exp val epsilon)
                 (check-equal? exp val)))
         #'(check-equal? expr val)))))

(provide
 #%module-begin
 #%top-interaction
 #%datum
 require
 planet
 only-in
 (rename-out
  (eq? EQ?)
  (al:#%app #%app)
  (al:#%top #%top)
  (ab:and AND)
  (ab:cond COND)
  (as:defun DEFUN)
;  (al:eval EVAL)
  (as:foreach FOREACH)
  (al:function FUNCTION)
  (ab:if IF)
  (as:lambda LAMBDA)
  (ab:or OR)
  (al:progn PROGN)
;  (al:quote QUOTE)
  (quote quote)
  (quote QUOTE)
  (al:repeat REPEAT)
  (as:setq SETQ)
  (ab:while WHILE)
  (using-input USING-INPUT)
  (check-equal-output? CHECK-EQUAL-OUTPUT?)
  (check-equal? CHECK-EQUAL?)
  (check-= CHECK-=)
  (check CHECK)
  (define-namespace-anchor DEFINE-NAMESPACE-ANCHOR)
  (namespace-anchor->namespace NAMESPACE-ANCHOR->NAMESPACE)
  (set-autolisp-namespace! SET-AUTOLISP-NAMESPACE!)
  (provide PROVIDE)
  (all-from-out ALL-FROM-OUT)
  ))

#|

  Dynamic scope

|#


(define autolisp-namespace #f)
  
(define (set-autolisp-namespace! namespace)
  (set! autolisp-namespace namespace))
  
(define global-parameters (make-hasheq))

(define (create-symbol-parameter! sym val)
  (let ((parameter (make-parameter val)))
    (hash-set! global-parameters sym parameter)
    parameter))

(define (get-symbol-parameter sym)
  (hash-ref global-parameters sym #f))

(define (symbol-parameter sym)
  (or (get-symbol-parameter sym)
      (create-symbol-parameter! sym (nil))))

(define (dynamic-ref sym)
  ((symbol-parameter sym)))

(define (dynamic-set! sym val)
  ((symbol-parameter sym) val)
  val)

(define (as:boundp sym)
  (if (nil? (dynamic-ref sym))
      (nil)
      (true)))

(define-syntax al:#%top
  (syntax-rules ()
    ((_ . sym)
     (dynamic-ref 'sym))))

(define-syntax (as:setq stx)
  (syntax-case stx ()
    ((_ sym expr)
     #'(dynamic-set! 'sym expr))
    ((_ sym expr others ...)
     #'(begin
         (as:setq sym expr)
         (as:setq others ...)))))

(define al:read
  (case-lambda
    (()
     (with-autolisp-reader-parameters read))
    ((str)
     (with-input-from-string str
       (lambda ()
         (with-autolisp-reader-parameters read))))))

(define (al:eval expr)
  (eval expr autolisp-namespace))

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

(define-syntax (provide-rename-upcase stx)
  (syntax-case stx ()
    ((_ (id out) ...)
     (syntax/loc stx
       (let ()
         (provide-rename-upcase-single id out) ...
         (void))))))

(define-syntax (provide-rename-upcase-single stx)
  (syntax-case stx ()
    ((_ id out)
     (with-syntax ((OUT
                    (string->symbol
                     (string-upcase
                      (symbol->string
                       (syntax->datum #'out))))))
       (syntax/loc stx
         (dynamic-set! 'OUT id))))))


(provide-rename-upcase
  (aa:+ +)
  (aa:- -)
  (aa:* *)
  (aa:/ /)
  (ab:= =)
  (ab:/= /=)
  (ab:< <)
  (ab:<= <=)
  (ab:> >)
  (ab:>= >=)
  (ab:~ ~)
  (aa:1+ 1+)
  (aa:1- 1-)
  (aa:abs abs)
  (al:acad_strlsort acad_strlsort)
;;  (ab:and and)
  (al:angle angle)
  (al:angtof angtof)
  (al:apply apply)
  (al:append append)
;;  (al:apply apply)
  (al:ascii ascii)
  (alab:assoc assoc)
  (al:atan atan)
  (al:atof atof)
  (al:atoi atoi)
  (al:atom atom)
  (as:boundp boundp)
  (al:caddr caddr)
  (al:cadr cadr)
  (al:car car)
  (al:cdr cdr)
  (al:chr chr)
  (al:close close)
 ;; (ab:cond cond)
  (al:cons cons)
  (al:cos cos)
 ;; (rs:defun defun)
  (al:distance distance)
  (al:distof distof)
  (ab:eq? eq)
  (ab:equal equal)
  (al:eval eval)
  (al:exp exp)
  (aa:expt expt)
  (al:fix fix)
  (al:float float)
;;  (al:foreach foreach)
;;  (al:function function)
  (al:gc gc)
  (al:gcd gcd)
;;  (ab:if if)
  (al:itoa itoa)
;;  (rs:lambda lambda)
  (al:last last)
  (al:length length)
  (al:list list)
  (alab:listp listp)
  (al:log log)
  (al:logand logand)
  (al:logior logior)
  (al:lsh lsh)
  (al:mapcar mapcar)
  (al:max max)
  (al:mem mem)
  (al:member member)
  (al:min min)
  (ab:minusp minusp)
  (alab:null? null)
;;  ('nil nil)  This is an exception
  (alab:not not)
  (al:nth nth)
  (ab:numberp numberp)
  (al:open open)
;;  (ab:or or)
  (al:pi pi)
  (al:prin1 prin1)
  (al:princ princ)
  (al:print print)
 ;; (al:progn progn)
 ;; (al:quote quote)
  (al:read read)
  (al:read-char read-char)
  (al:read-line read-line)
  (al:rem rem)
 ;; (al:repeat repeat)
  (al:reverse reverse)
 ;; (rs:setq setq)
  (al:sin sin)
  (al:sqrt sqrt)
  (al:strcase strcase)
  (al:strcat strcat)
  (al:strlen strlen)
  (al:subst subst)
  (al:substr substr)
  ('T T)
  (al:terpri terpri)
  (al:type type)
  (al:ver ver)
 ;; (al:while while)
  (al:write-char write-char)
  (al:write-line write-line)
  (ab:zerop zerop))

;;(dynamic-set! 'nil 'nil)
;;(dynamic-set! 'T 'T)

(define-syntax rs:setq
  (syntax-rules ()
    ((_ sym expr)
     (rs:define sym expr))
    ((_ sym expr others ...)
     (begin
       (rs:define sym expr)
       (rs:setq others ...)))))

(define-syntax as:defun
  (syntax-rules ()
    ((_ name (var ...) expr ...)
     (as:setq name (as:lambda (var ...) expr ...)))))

(define-syntax (as:lambda stx)
  (define (not-/? stx)
    (not (eq? (syntax->datum stx) '/)))
  (syntax-case stx ()
    ((as:lambda (sym ...) expr ...)
     (let-values (((params /locals) (splitf-at (syntax->list #'(sym ...)) not-/?)))
       (let ((locals (if (null? /locals) (list) (cdr /locals))))
         (with-syntax (((par ...) params)
                       ((loc ...) locals))
           (with-syntax
               (((t ...) (generate-temporaries #'(par ...))))
             (syntax/loc stx
               (lambda (t ...)
                 (parameterize (((symbol-parameter 'par) t) ...
                                ((symbol-parameter 'loc) 'nil) ...)
                   expr ...))))))))))


(define-syntax (parameterized-lambda stx)
  (syntax-case stx ()
    ((_ (par ...) (loc ...) (expr ...))
     (with-syntax
         (((t ...) (generate-temporaries #'(par ...))))
       #'(lambda (t ...)
           (parameterize (((symbol-parameter 'par) t) ...
                          ((symbol-parameter 'loc) 'nil) ...)
             expr ...))))))


(define-syntax as:foreach
  (syntax-rules ()
    ((foreach name expr form0 forms ...)
     (let loop ((l expr) (res (nil)))
       (if (null? l)
           res
           (loop (cdr l)
                 (parameterize (((symbol-parameter 'name) (car l)))
                   form0 forms ...)))))))



;; ;;It might be better to leave this to the reader
;; (define-syntax (al:quote stx)
;;   (define (null-or-nil? obj)
;;     (or (null? obj) (nil? obj)))
;;   (define (null-list->nil-list l)
;;     (cond ((null-or-nil? l)
;;            '())
;;           ((not (pair? l))
;;            l)
;;           ((null-or-nil? (car l))
;;            (cons (nil) (null-list->nil-list (cdr l))))
;;           (else
;;            (cons (null-list->nil-list (car l))
;;                  (null-list->nil-list (cdr l))))))
;;   (syntax-case stx ()
;;     ((tmpl ())
;;      (datum->syntax #'tmpl (nil)))
;;     ((_ (car . cdr))
;;      #`'#,(null-list->nil-list (syntax->datum #'(car . cdr))))
;;     ((tmpl datum)
;;      #''datum)))


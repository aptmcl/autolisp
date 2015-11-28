(module reader syntax/module-reader
  #:language "autolisp"
  #:wrapper1 with-autolisp-reader-parameters
  (require "readtable.rkt"))

#|
;;#lang s-exp syntax/module-reader
;;autolisp

(module reader syntax/module-reader
  #:language "asaaabal.ss"

  #:read (upcase-reader read)

  #:read-syntax (upcase-syntax-reader read-syntax)

  ;;ERROR.  MUST CHECK LISTS
  (define ((upcase-reader reader) . args)
    (parameterize ([read-decimal-as-inexact #t])
      (let ((dat (apply reader args)))
        (if (symbol? dat)
            (string->symbol (string-upcase (symbol->string dat)))
            dat))))
  
  (define ((upcase-syntax-reader reader) . args)
    (parameterize ([read-decimal-as-inexact #t])
      (let ((stx (apply reader args)))
        (if (eof-object? stx)
            stx
            (let ((dat (syntax->datum stx)))
              (if (symbol? dat)
                  (datum->syntax stx (string->symbol (string-upcase (symbol->string dat))))
                  dat)))))))
|#
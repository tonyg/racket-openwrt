#lang typed/racket/base

;; (require racket/base
;; 	 [exn:fail:contract (String Any -> 

(provide define-mapping define-mapping:)

(define-syntax check-defaults
  (syntax-rules ()
    ((_ fn bn fd bd #:forward-default new-fd rest ...)
     (check-defaults fn bn new-fd bd rest ...))
    ((_ fn bn fd bd #:backward-default new-bd rest ...)
     (check-defaults fn bn fd new-bd rest ...))
    ((_ fn bn fd bd (lhs rhs) ...)
     (begin
       (define (fn l)
	 (case l
	   ((lhs) 'rhs) ...
	   (else (fd l))))
       (define (bn r)
	 (case r
	   ((rhs) 'lhs) ...
	   (else (bd r))))))))

(define (die-with-mapping-name n)
  (lambda (v)
    (raise (exn:fail:contract
	    (format "~v: Mapping not found for ~v" n v)
	    (current-continuation-marks)))))

(define-syntax define-mapping
  (syntax-rules ()
    ((_ forward-name backward-name rest ...)
     (check-defaults forward-name
		     backward-name
		     (die-with-mapping-name 'forward-name)
		     (die-with-mapping-name 'backward-name)
		     rest ...))))

(define-syntax check-defaults:
  (syntax-rules ()
    ((_ tn unt fn bn fd bd #:forward-default new-fd rest ...)
     (check-defaults: tn unt fn bn new-fd bd rest ...))
    ((_ tn unt fn bn fd bd #:backward-default new-bd rest ...)
     (check-defaults: tn unt fn bn fd new-bd rest ...))
    ((_ tn unt fn bn fd bd (lhs rhs) ...)
     (begin
       (define-type tn (U unt 'lhs ... 'rhs ...))
       (define (fn l)
	 (case l
	   ((lhs) 'rhs) ...
	   (else (fd l))))
       (define (bn r)
	 (case r
	   ((rhs) 'lhs) ...
	   (else (bd r))))))))

(define-syntax define-mapping:
  (syntax-rules ()
    ((_ TypeName UnderlyingNumericType forward-name backward-name rest ...)
     (check-defaults: TypeName
		      UnderlyingNumericType
		      forward-name
		      backward-name
		      (die-with-mapping-name 'forward-name)
		      (die-with-mapping-name 'backward-name)
		      rest ...))))

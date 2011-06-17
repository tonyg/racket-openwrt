(define-structure assert
  (export (assert :syntax))
  (open scheme)
  (open srfi-23) ;; error
  (begin (define-syntax assert
	   (syntax-rules ()
	     ((_ test)
	      (if (not test) (error "Failed assertion" 'test)))))))

(define-structure gensym
  (export gensym)
  (open scheme)
  (begin (define gensym
	   (let ((counter 14641))
	     (lambda (prefix-sym)
	       (string->symbol (string-append (symbol->string prefix-sym)
					      (let ((v counter))
						(set! counter (+ counter 1))
						(number->string v)))))))))

(define-structure dircompat
  (export current-directory
	  path->string
	  string->path)
  (open scheme)
  (open posix)
  (open os-strings)
  (begin (define (current-directory)
	   (string-append (os-string->string (working-directory)) "/"))
	 (define path->string values)
	 (define string->path values)))

(define-structure ioutils
  (export read-line
	  port->lines
	  port->string
	  display-lines
	  eof
	  flush-output)
  (open scheme)
  (open (subset primitives (eof-object)))
  (files "ioutils.scm"))

(define-structure process
  (export process
	  process*
	  simple-pipeline)
  (open scheme)
  (open posix)
  (open srfi-23) ;; error
  (open srfi-34) ;; exceptions
  (open ioutils)
  (files "process.scm"))

(define-structure uci
  (export uci-package
	  uci-package?
	  uci-package-name
	  uci-package-sections

	  uci-section
	  uci-section?
	  uci-section-name
	  uci-section-type
	  uci-section-options

	  uci-package-get-section
	  uci-package-set-section
	  uci-package-delete-section
	  uci-section-get
	  uci-section-set
	  uci-section-delete
	  uci-package-get
	  uci-package-set
	  uci-package-update

	  package->lines
	  lines->package
	  uci-program-name
	  run-uci
	  uci-read-package
	  uci-read-packages)
  (open scheme)
  (open srfi-1)
  (open srfi-9)
  (open srfi-13)
  (open srfi-23)
  (open srfi-39)
  (open assert)
  (open process)
  (open ioutils)
  (open gensym)
  (open dircompat)
  (open i/o)
  (files "uci.scm"))

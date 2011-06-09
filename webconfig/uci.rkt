#lang racket

(require srfi/13)
(require (only-in rnrs assert))

(provide (struct-out uci-package)
	 (struct-out uci-section)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UCI configuration packages and sections.

(struct uci-package (name sections) #:transparent)
(struct uci-section (name type options) #:transparent)

(define (uci-package-get-section package section-name
				 #:if-absent
				 [if-absent (lambda ()
					      (error 'uci-package-get-section
						     "Missing section ~s in package ~s"
						     section-name
						     (uci-package-name package)))])
  (cond
   ((memf (lambda (section) (equal? (uci-section-name section) section-name))
	  (uci-package-sections package)) => car)
   (else (if (procedure? if-absent)
	     (if-absent)
	     if-absent))))

(define (remove-section sections section-name)
  (filter (lambda (section) (not (equal? (uci-section-name section) section-name)))
	  sections))

(define (uci-package-set-section package section)
  (uci-package (uci-package-name package)
	       (append (remove-section (uci-package-sections package) (uci-section-name section))
		       (list section))))

(define (uci-package-delete-section package section-name)
  (uci-package (uci-package-name package)
	       (remove-section (uci-package-sections package) section-name)))

(define (uci-section-get section option-name
			 #:if-absent [if-absent (lambda ()
						  (error 'uci-section-get
							 "Missing option ~s in section ~s"
							 option-name
							 (uci-section-name section)))])
  (cond
   ((assoc option-name (uci-section-options section)) => cadr)
   (else (if (procedure? if-absent)
	     (if-absent)
	     if-absent))))

(define (remove-option options option-name)
  (filter (lambda (kv) (not (equal? (first kv) option-name)))
	  options))

(define (uci-section-set section option-name value)
  (uci-section (uci-section-name section)
	       (uci-section-type section)
	       (append (remove-option (uci-section-options section) option-name)
		       (list (list option-name value)))))

(define (uci-section-delete section option-name)
  (uci-section (uci-section-name section)
	       (uci-section-type section)
	       (remove-option (uci-section-options section) option-name)))

(define (uci-package-get package section-name option-name)
  (uci-section-get (uci-package-get package section-name) option-name))

(define (uci-package-set package section-name option-name value)
  (uci-package-set package
		   section-name
		   (uci-section-set (uci-package-get package section-name)
				    option-name
				    value)))

(define (uci-package-update package section-name option-name f
			    #:if-absent [if-absent (lambda ()
						     (error 'uci-section-get
							    "Missing option ~s.~s.~s"
							    (uci-package-name package)
							    section-name
							    option-name))])
  (let ((old-section (uci-package-get package section-name)))
    (uci-package-set package
		     section-name
		     (uci-section-set old-section
				      option-name
				      (f (uci-section-get old-section option-name
							  #:if-absent if-absent))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization and parsing of packages and sections.

(define (format-config-string str)
  (list->string
   (cons #\space
	 (cons #\'
	       (foldr (lambda (chr acc)
			(case chr
			  ((#\') (append (string->list "'\\''") acc))
			  (else (cons chr acc))))
		      (list #\')
		      (string->list (if (symbol? str)
					(symbol->string str)
					str)))))))

(define (format-line indent? keyword . args)
  (string-append* (if indent? "\t" "")
		  (symbol->string keyword)
		  (map format-config-string args)))

(define (package->lines package)
  (cons (format-line #f 'package (uci-package-name package))
	(append-map section->lines (uci-package-sections package))))

(define (section->lines section)
  (cons (format-line #f 'config (uci-section-type section) (uci-section-name section))
	(append-map option->lines (uci-section-options section))))

(define (option->lines kv)
  (if (list? (second kv))
      (map (lambda (v) (format-line #t 'list (first kv) v)) (second kv))
      (list (format-line #t 'option (first kv) (second kv)))))

(define (split-line line)
  (define (bad)
    (error 'split-line "Syntax error in line ~s" line))
  (define (lex-word pieces-rev chars)
    (if (null? chars)
	(reverse pieces-rev)
	(let ((ch (first chars)))
	  (cond
	   ((char-whitespace? ch) (lex-word pieces-rev (rest chars)))
	   ((char-alphabetic? ch) (lex-bareword pieces-rev '() chars))
	   ((eqv? ch #\') (lex-quoted pieces-rev '() (rest chars)))
	   (else (bad))))))
  (define (lex-bareword pieces-rev acc chars)
    (if (and (pair? chars)
	     (or (char-alphabetic? (first chars))
		 (char-numeric? (first chars))
		 (eqv? (first chars) #\_)))
	(lex-bareword pieces-rev (cons (first chars) acc) (rest chars))
	(lex-word (cons (list->string (reverse acc)) pieces-rev) chars)))
  (define (lex-quoted pieces-rev acc chars)
    (match chars
      ('() (bad))
      (`(#\' #\\ ,c #\' . ,rest)
       (lex-quoted pieces-rev (cons c acc) rest))
      (`(#\' . ,rest)
       (lex-word (cons (list->string (reverse acc)) pieces-rev) rest))
      (`(,c . ,rest)
       (lex-quoted pieces-rev (cons c acc) rest))))
  (lex-word '() (string->list line)))

(define (unformat-line old-lines k)
  (if (null? old-lines)
      (k old-lines eof eof)
      (match (split-line (first old-lines))
	('()
	 (unformat-line (rest old-lines) k))
	((cons keyword-str pieces)
	 (k (rest old-lines) (string->symbol keyword-str) pieces)))))

(define (lines->option old-lines k)
  (unformat-line old-lines
   (lambda (new-lines keyword pieces)
     (cond
      ((eq? keyword 'list) (lines->list new-lines
					(string->symbol (first pieces))
					(list (second pieces))
					k))
      ((eq? keyword 'option) (k new-lines
				(string->symbol (first pieces))
				(second pieces)))
      (else (k old-lines eof eof))))))

(define (lines->list old-lines name list-values k)
  (unformat-line old-lines
   (lambda (new-lines keyword pieces)
     (cond
      ((and (eq? keyword 'list) (eq? name (string->symbol (first pieces))))
       (lines->list new-lines name (cons (second pieces) list-values) k))
      (else (k old-lines name (reverse list-values)))))))

(define (lines->section old-lines k)
  (unformat-line old-lines
   (lambda (new-lines keyword pieces)
     (cond
      ((eq? keyword 'config)
       (lines->section* new-lines
			(string->symbol (first pieces))
			(if (> (length pieces) 1)
			    (string->symbol (second pieces))
			    (gensym 'cfg))
			'()
			k))
      (else (k old-lines eof))))))

(define (lines->section* old-lines type name options k)
  (lines->option old-lines
   (lambda (new-lines option-name option-value)
     (cond
      ((eof-object? option-name)
       (k old-lines (uci-section name type (reverse options))))
      (else (lines->section* new-lines
			     type
			     name
			     (cons (list option-name option-value) options)
			     k))))))

(define (lines-accumulate old-lines produce-one k)
  (produce-one old-lines
   (lambda (new-lines entity)
     (if (eof-object? entity)
	 (k old-lines '())
	 (lines-accumulate new-lines produce-one
	  (lambda (new-lines entities)
	    (k new-lines (cons entity entities))))))))

(define (lines->sections old-lines k)
  (lines-accumulate old-lines lines->section k))

(define (lines->package old-lines k)
  (unformat-line old-lines
   (lambda (new-lines keyword pieces)
     (cond
      ((eq? keyword 'package)
       (lines->sections new-lines
        (lambda (new-lines sections)
	  (k new-lines (uci-package (string->symbol (first pieces)) sections)))))
      (else (k old-lines eof))))))

(define (lines->packages old-lines k)
  (lines-accumulate old-lines lines->package k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking the UCI command-line tool.

;; TODO: switch over for real
;;(define uci-program-name (make-parameter (list "/usr/bin/env" "uci")))
(define uci-program-name
  (let ((cwd-rel (lambda (x) (string-append (path->string (current-directory)) x))))
    (make-parameter (list (cwd-rel "../uci/uci/uci")
			  (string-append "-c" (cwd-rel "../uci/examples"))))))

(define (run-uci args input-lines)
  (match (apply process* (append (uci-program-name) args))
    (`(,stdout ,stdin ,pid ,stderr ,controller)
     (display-lines input-lines stdin)
     (flush-output stdin)
     (close-output-port stdin)
     (controller 'wait)
     (display (port->string stderr) (current-error-port))
     (match (controller 'status)
       ('done-ok (port->lines stdout))
       ('done-error #f)))))

(define (uci-read-package package-name)
  (lines->package (run-uci `("-n" "export" ,(symbol->string package-name)) '())
   (lambda (remaining-lines package)
     package)))

(define (uci-read-packages)
  (lines->packages (run-uci `("-n" "export") '())
   (lambda (remaining-lines packages)
     packages)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing.

(define-syntax test-begin
  (syntax-rules ()
    ((_ exp ...)
     (parameterize ((uci-program-name (list "../uci/uci/uci" "-c../uci/examples")))
       exp ...))))

(assert (equal? (split-line "a 'b' 'c'\\''d' 'e'f'g''h'")
		'("a" "b" "c'd" "e" "f" "g" "h")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UCI configuration packages and sections.

(define-record-type <uci-package>
  (uci-package name sections)
  uci-package?
  (name uci-package-name)
  (sections uci-package-sections))

(define-record-type <uci-section>
  (uci-section name type options)
  uci-section?
  (name uci-section-name)
  (type uci-section-type)
  (options uci-section-options))

(define (extract-if-absent maybe-if-absent thunk)
  (if (null? maybe-if-absent)
      thunk
      (car maybe-if-absent)))

(define (run-if-absent maybe-if-absent thunk)
  (let ((v (extract-if-absent maybe-if-absent thunk)))
    (if (procedure? v) (v) v)))

(define (uci-package-get-section package section-name . maybe-if-absent)
  (cond
   ((find (lambda (section) (equal? (uci-section-name section) section-name))
	  (uci-package-sections package)))
   (else (run-if-absent maybe-if-absent
			(lambda ()
			  (error 'uci-package-get-section
				 "Missing section ~s in package ~s"
				 section-name
				 (uci-package-name package)))))))

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

(define (uci-section-get section option-name . maybe-if-absent)
  (cond
   ((assoc option-name (uci-section-options section)) => cadr)
   (else (run-if-absent maybe-if-absent
			(lambda ()
			  (error 'uci-section-get
				 "Missing option ~s in section ~s"
				 option-name
				 (uci-section-name section)))))))

(define (remove-option options option-name)
  (filter (lambda (kv) (not (equal? (car kv) option-name)))
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

(define (uci-package-update package section-name option-name f . maybe-if-absent)
  (let ((old-section (uci-package-get package section-name))
	(if-absent (extract-if-absent maybe-if-absent
				      (lambda ()
					(error 'uci-section-get
					       "Missing option ~s.~s.~s"
					       (uci-package-name package)
					       section-name
					       option-name)))))
    (uci-package-set package
		     section-name
		     (uci-section-set old-section
				      option-name
				      (f (uci-section-get old-section option-name
							  if-absent))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization and parsing of packages and sections.

(define (format-config-string str)
  (list->string
   (cons #\space
	 (cons #\'
	       (fold-right (lambda (chr acc)
			     (case chr
			       ((#\') (append (string->list "'\\''") acc))
			       (else (cons chr acc))))
			   (list #\')
			   (string->list (if (symbol? str)
					     (symbol->string str)
					     str)))))))

(define (format-line indent? keyword . args)
  (string-concatenate (cons (if indent? "\t" "")
			    (cons (symbol->string keyword)
				  (map format-config-string args)))))

(define (package->lines package)
  (cons (format-line #f 'package (uci-package-name package))
	(append-map section->lines (uci-package-sections package))))

(define (section->lines section)
  (cons (format-line #f 'config (uci-section-type section) (uci-section-name section))
	(append-map option->lines (uci-section-options section))))

(define (option->lines kv)
  (if (list? (cadr kv))
      (map (lambda (v) (format-line #t 'list (car kv) v)) (cadr kv))
      (list (format-line #t 'option (car kv) (cadr kv)))))

(define (split-line line)
  (define (bad)
    (error 'split-line "Syntax error in line ~s" line))
  (define (lex-word pieces-rev chars)
    (if (null? chars)
	(reverse pieces-rev)
	(let ((ch (car chars)))
	  (cond
	   ((char-whitespace? ch) (lex-word pieces-rev (cdr chars)))
	   ((char-alphabetic? ch) (lex-bareword pieces-rev '() chars))
	   ((eqv? ch #\') (lex-quoted pieces-rev '() (cdr chars)))
	   (else (bad))))))
  (define (lex-bareword pieces-rev acc chars)
    (if (and (pair? chars)
	     (or (char-alphabetic? (car chars))
		 (char-numeric? (car chars))
		 (eqv? (car chars) #\_)))
	(lex-bareword pieces-rev (cons (car chars) acc) (cdr chars))
	(lex-word (cons (list->string (reverse acc)) pieces-rev) chars)))
  (define (lex-quoted pieces-rev acc chars)
    (cond
     ((null? chars) (bad))
     ((eqv? (car chars) #\')
      (if (and (pair? (cdr chars))
	       (pair? (cddr chars))
	       (pair? (cdddr chars))
	       (eqv? (cadr chars) #\\)
	       (eqv? (cadddr chars) #\'))
	  (lex-quoted pieces-rev (cons (caddr chars) acc) (cddddr chars))
	  (lex-word (cons (list->string (reverse acc)) pieces-rev) (cdr chars))))
     (else
      (lex-quoted pieces-rev (cons (car chars) acc) (cdr chars)))))
  (lex-word '() (string->list line)))

(define (unformat-line old-lines k)
  (if (null? old-lines)
      (k old-lines eof eof)
      (let ((result (split-line (car old-lines))))
	(if (null? result)
	    (unformat-line (cdr old-lines) k)
	    (k (cdr old-lines)
	       (string->symbol (car result))
	       (cdr result))))))

(define (lines->option old-lines k)
  (unformat-line old-lines
   (lambda (new-lines keyword pieces)
     (cond
      ((eq? keyword 'list) (lines->list new-lines
					(string->symbol (car pieces))
					(list (cadr pieces))
					k))
      ((eq? keyword 'option) (k new-lines
				(string->symbol (car pieces))
				(cadr pieces)))
      (else (k old-lines eof eof))))))

(define (lines->list old-lines name list-values k)
  (unformat-line old-lines
   (lambda (new-lines keyword pieces)
     (cond
      ((and (eq? keyword 'list) (eq? name (string->symbol (car pieces))))
       (lines->list new-lines name (cons (cadr pieces) list-values) k))
      (else (k old-lines name (reverse list-values)))))))

(define (lines->section old-lines k)
  (unformat-line old-lines
   (lambda (new-lines keyword pieces)
     (cond
      ((eq? keyword 'config)
       (lines->section* new-lines
			(string->symbol (car pieces))
			(if (> (length pieces) 1)
			    (string->symbol (cadr pieces))
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
	  (k new-lines (uci-package (string->symbol (car pieces)) sections)))))
      (else (k old-lines eof))))))

(define (lines->packages old-lines k)
  (lines-accumulate old-lines lines->package k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking the UCI command-line tool.

(define uci-program-name (make-parameter (list "/usr/bin/env" "uci")))

(define (run-uci args input-lines)
  (call-with-values (lambda ()
		      (apply values (apply process* (append (uci-program-name) args))))
    (lambda (stdout stdin pid stderr controller)
      (display-lines input-lines stdin)
      (flush-output stdin)
      (close-output-port stdin)
      (controller 'wait)
      (display (port->string/close stderr) (current-error-port))
      (case (controller 'status)
	((done-ok) (port->lines/close stdout))
	(else (close-input-port stdout)
	      #f)))))

(define (uci-read-package package-name)
  (lines->package (run-uci `("-n" "export" ,(symbol->string package-name)) '())
   (lambda (remaining-lines package)
     package)))

(define (uci-read-packages)
  (lines->packages (run-uci `("-n" "export") '())
   (lambda (remaining-lines packages)
     packages)))

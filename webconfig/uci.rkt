#lang racket

(require srfi/13)
(require (only-in rnrs assert))

(provide uci-program-name
	 run-uci
	 uci-descriptions
	 (struct-out uci-section))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UCI configuration sections

(struct uci-section (config name type options) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking the UCI command-line tool.

(define uci-program-name (make-parameter (list "/usr/bin/env" "uci")))

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

;; TODO: uses randomness. Security?
(define (fresh-separator)
  (do ((i 0 (+ i 1))
       (acc 0 (+ (* acc 65536) (random 65536))))
      ((= i 4) (string-append "--" (number->string acc 16) "--"))))

(define (format-uci-segment seg)
  (cond
   ((false? seg) '())
   ((symbol? seg) (list "." (symbol->string seg)))
   ((list? seg) (list ".@" (symbol->string (first seg))
		      "[" (number->string (second seg)) "]"))
   (else (error 'format-uci-segment "Unsupported segment ~s" seg))))

(define (format-option-name config [section #f] [option #f])
  (if (false? config)
      '()
      (list
       (string-append* (symbol->string config)
		       (append (format-uci-segment section)
			       (format-uci-segment option))))))

(define (uci-read [config #f] [section #f] [option #f])
  (let* ((separator (fresh-separator))
	 (lines (run-uci `(,(string-append "-d" separator)
			   "show"
			   ,@(format-option-name config section option))
			 '())))
    (build-result (reverse lines) (make-splitter separator))))

(define (make-splitter separator)
  (let ((rx (regexp (regexp-quote separator))))
    (lambda (str) (regexp-split rx str))))

(define parse-key
  (let ((dot-rx (regexp (regexp-quote ".")))
	;; Regexp syntax sucks. Oh, for SREs!
	(aref-rx (regexp "@([^[]*)\\[([^]]*)\\]")))
    (lambda (str)
      (map (lambda (piece)
	     (match (regexp-match aref-rx piece)
	       (#f (string->symbol piece))
	       (`(,_ ,classname ,indexstr)
		(list (string->symbol classname) (string->number indexstr)))))
	   (regexp-split dot-rx str)))))

(define (parse-line line splitter)
  (let* ((=pos (string-index line #\=))
	 (keystr (substring line 0 =pos))
	 (valstr (substring line (+ =pos 1))))
    (cons (parse-key keystr)
	  (splitter valstr))))

(define (build-result lines-rev splitter)
  (let loop ((lines-rev lines-rev)
	     (sections '())
	     (section-name #f)
	     (section '()))
    (if (null? lines-rev)
	(begin
	  (assert (null? section))
	  (assert (false? section-name))
	  sections)
	(match (parse-line (car lines-rev) splitter)
	  ((cons key vals)
	   (assert (or (false? section-name) (equal? section-name (second key))))
	   (case (length key)
	     ((2) ;; a section type definition
	      (loop (cdr lines-rev)
		    (cons (uci-section (first key) ;; config name
				       (second key) ;; section name
				       (first vals) ;; section type
				       section)
			  sections)
		    #f
		    '()))
	     ((3) ;; an option definition
	      (loop (cdr lines-rev)
		    sections
		    (second key)
		    (cons (cons (third key) vals) section)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile-time loading of the UCI description JSON data.

(require (for-syntax "json-parsing.ss"))

(define-syntax loaded-uci-descriptions
  (lambda (stx)
    (syntax-case stx ()
      ((_) #`(quote #,(call-with-input-file "uci-descriptions.js"
			(lambda (f) (json->sjson f))))))))

(define uci-descriptions (make-parameter (loaded-uci-descriptions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing.

(define-syntax test-begin
  (syntax-rules ()
    ((_ exp ...)
     (parameterize ((uci-program-name (list "../uci/uci/uci" "-c../uci/examples")))
       exp ...))))

(require test-engine/racket-tests)
;;(check-expect (test-begin (run-uci `("show" "dhcp") `())) '())
(check-expect (parse-line "dhcp.@dnsmasq[0].authoritative=1" (make-splitter "--"))
	      '((dhcp (dnsmasq 0) authoritative) "1"))
(check-expect (parse-line "dhcp.@dnsmasq[0].authoritative=1--2--3" (make-splitter "--"))
	      '((dhcp (dnsmasq 0) authoritative) "1" "2" "3"))
(test)

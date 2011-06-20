(define dispatch '*)
(define entry-url '*)

(define-syntax page
  (syntax-rules ()
    ((_ title-expr page-class body-xhtml ...)
     (let ((title-val title-expr))
       (make-http-response 200 "OK" `((content-type "text/html"))
	(xxexpr->string
	 `((html (head (title "OpenWRT Racket Configurator: " ,title-val)
		       (link ((rel "stylesheet")
			      (href "/style.css")
			      (type "text/css"))))
		 (body ((class ,page-class))
		       (div ((class ,page-class))
			    (h1 ,title-val)
			    (ul ((class "navigation"))
				(li (a ((href ,(entry-url main))) "index"))
				(li (a "test1"))
				(li (a "test2")))
			    body-xhtml ...))))))))))

;; (define-syntax send-page
;;   (syntax-rules ()
;;     ((_ make-url title page-class body ...)
;;      (send/suspend/dispatch
;;       (lambda (make-url)
;; 	(page title page-class body ...))))))

(define-syntax send-page
  (syntax-rules ()
    ((_ make-url title page-class body ...)
     (let ((make-url (lambda args (error "make-url not yet supported"))))
       (page title page-class body ...)))))

(define (main req)
  (send-page make-url "Available Packages" "package-index"
	     (ul ,@(map (lambda (package)
			  (let ((n (symbol->string (uci-package-name package))))
			    `(li (a ((href ,(entry-url show-uci-package n))) ,n))))
			(uci-read-packages)))))

(define (show-uci-package req package-name-string)
  (send-page make-url package-name-string "package-detail"
	     ,@(map section->div (uci-package-sections
				  (uci-read-package
				   (string->symbol package-name-string))))))

(define (safe-path pieces)
  (string-concatenate
   (interleave-element "/"
    (let loop ((pieces pieces))
      (if (null? pieces)
	  '()
	  (let ((piece (car pieces)))
	    (cond
	     ((string=? piece "") (error "Empty path piece"))
	     ((char=? (string-ref piece 0) #\.) (error "Forbidden path piece"))
	     (else (cons piece (loop (cdr pieces)))))))))))

(define (serve-file relative-path)
  (make-http-response 200 "OK" '() ;; TODO - content type
		      (call-with-input-file relative-path port->string)))

(define (static-content req)
  (serve-file (safe-path (parsed-path-pieces (http-request-parsed-path req)))))

(define (section->div section)
  `(div ((class "config-section"))
	(h2 (span ((class "config-section-type"))
		  ,(symbol->string (uci-section-type section)))
	    (span ((class "config-section-name"))
		  ,(symbol->string (uci-section-name section))))
	(dl ,@(append-map option->dt+dd (uci-section-options section)))))

(define (option->dt+dd kv)
  (if (list? (cadr kv))
      `((dt ((class "config-list")) ,(symbol->string (car kv)))
	(dd ((class "config-list"))
	    (ul ,@(map (lambda (v) `(li ,v)) (cadr kv)))))
      `((dt ((class "config-option")) ,(symbol->string (car kv)))
	(dd ((class "config-option")) ,(cadr kv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(call-with-values
    (lambda () (dispatch-rules
		(("index.html") main)
		(("") main)
		(("page" "show" (string-arg)) show-uci-package)
		(else static-content)))
  (lambda (dr du)
    (set! dispatch dr)
    (set! entry-url du)))

(define running-on-the-real-hardware?
  (call-with-values (lambda () (simple-pipeline "" "/usr/bin/env" "uname" "-m"))
    (lambda (output error-output)
      (string=? (string-trim-both output) "mips"))))

(define port-number (if running-on-the-real-hardware? 80 8000))

(if (not running-on-the-real-hardware?)
    (let ((cwd-rel (lambda (x) (string-append (path->string (current-directory)) x))))
      (display "Using the fake uci")
      (uci-program-name (list (cwd-rel "../uci/uci/uci")
			      (string-append "-c" (cwd-rel "../uci/examples")))))
    (display "Using the real uci"))
(newline)

(for-each display (list "Running on port "port-number"..."))
(newline)
(define daemon (make-http-daemon port-number dispatch))
(run-http-daemon daemon)

#!/usr/bin/env racket
#lang racket/base

(require web-server/servlet
         web-server/servlet-env
	 racket/list)

(require "uci.rkt")

(define-syntax page
  (syntax-rules ()
    ((_ title-expr page-class body-xhtml ...)
     (let ((title-val title-expr))
       (response/xexpr
	`(html (head (title "OpenWRT Racket Configurator: " ,title-val)
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
			  body-xhtml ...))))))))

(define-syntax send-page
  (syntax-rules ()
    ((_ make-url title page-class body ...)
     (send/suspend/dispatch
      (lambda (make-url)
	(page title page-class body ...))))))

(define-values (dispatch entry-url)
  (dispatch-rules
   (("") main)
   (("page" "show" (string-arg)) show-uci-package)
   (else main)))

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

(define (section->div section)
  `(div ((class "config-section"))
	(h2 (span ((class "config-section-type"))
		  ,(symbol->string (uci-section-type section)))
	    (span ((class "config-section-name"))
		  ,(symbol->string (uci-section-name section))))
	(dl ,@(append-map option->dt+dd (uci-section-options section)))))

(define (option->dt+dd kv)
  (if (list? (second kv))
      `((dt ((class "config-list")) ,(symbol->string (first kv)))
	(dd ((class "config-list"))
	    (ul ,@(map (lambda (v) `(li ,v)) (second kv)))))
      `((dt ((class "config-option")) ,(symbol->string (first kv)))
	(dd ((class "config-option")) ,(second kv)))))

(serve/servlet dispatch
	       #:launch-browser? #f
	       #:servlet-regexp #rx"/($|page/)"
	       #:extra-files-paths (list "."))

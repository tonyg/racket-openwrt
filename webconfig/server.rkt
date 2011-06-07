#!/usr/bin/env racket -f

(require web-server/servlet
         web-server/servlet-env)

(define-syntax page
  (syntax-rules ()
    ((_ title-expr body-xhtml ...)
     (let ((title-val title-expr))
       (response/xexpr
	`(html (head (title "OpenWRT Racket Configurator: " ,title-val)
		     (link ((rel "stylesheet")
			    (href "/style.css")
			    (type "text/css"))))
	       (body (h1 ,title-val)
		     body-xhtml ...)))))))

(define-syntax send-page
  (syntax-rules ()
    ((_ make-url title body ...)
     (send/suspend/dispatch
      (lambda (make-url)
	(page title body ...))))))

(define-values (dispatch entry-url)
  (dispatch-rules
   (("") main)
   (("page" "page1") page1)
   (("page" "page2") page2)
   (else main)))

(define (page1 req)
  (send-page make-url
	     "Page 1"
	     (p "Hello")
	     (a ((href ,(make-url page2)))
		"page 2")))

(define (page2 req)
  (send-page make-url
	     "Page 2"
	     (p "Goodbye2")
	     (a ((href ,(entry-url page1)))
		"page 1")))

(define (main req)
  (page1 req))

(serve/servlet dispatch
	       #:launch-browser? #f
	       #:servlet-regexp #rx"/($|page/)"
	       #:extra-files-paths (list "."))

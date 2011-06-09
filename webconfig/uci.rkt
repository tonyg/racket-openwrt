#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile-time loading of the UCI description JSON data.

(require (for-syntax "json-parsing.ss"))

(define-syntax loaded-uci-descriptions
  (lambda (stx)
    (syntax-case stx ()
      ((_) #`(quote #,(call-with-input-file "uci-descriptions.js"
			(lambda (f) (json->sjson f))))))))

(define uci-descriptions (loaded-uci-descriptions))
(provide uci-descriptions)

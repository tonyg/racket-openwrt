#lang racket/base

(require (only-in rnrs assert))

(provide uci-descriptions)

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

;; (require test-engine/racket-tests)
;; (test)

#lang racket/base

(require test-engine/racket-tests)

(require "json-parsing.ss")
(require "json-unparsing.rkt")

(define (roundtrip-equal? sjson)
  (json-equal? (json->sjson (sjson->json sjson)) sjson))

(check-expect (sjson->json #hash((a . 1))) "{\"a\":1}")
(check-expect (sjson->json '(1 "hello" #t #\null)) "[1,\"hello\",true,null]")

(check-expect (json-equal? 1.0 1.0) #t)
(check-expect (json-equal? 1.0 1.1) #f)
(check-expect (json-equal? 1.0 1) #t)
(check-expect (json-equal? 1 1) #t)
(check-expect (json-equal? 1 10) #f)
(check-expect (json-equal? 0.0 0) #t)
(check-expect (json-equal? '(1 2 3) '(1 2)) #f)
(check-expect (json-equal? '(1 2 3) '(1 2 3)) #t)
(check-expect (json-equal? '(1 2 3) '(2 1 3)) #f)
(check-expect (json-equal? '() '()) #t)
(check-expect (json-equal? '() '(1)) #f)
(check-expect (json-equal? #t #t) #t)
(check-expect (json-equal? #t #f) #f)
(check-expect (json-equal? #f #t) #f)
(check-expect (json-equal? #f #f) #t)
(check-expect (json-equal? "abc" "abc") #t)
(check-expect (json-equal? "abc" "ABC") #f)
(check-expect (json-equal? #hash((a . 1)) #hash((a . 2))) #f)
(check-expect (json-equal? #hash((a . 1)) #hash((b . 1))) #f)
(check-expect (json-equal? #hash((a . 1)) #hash((a . 1))) #t)
(check-expect (json-equal? #hash((a . 1)) #hash((a . 1) (b . 2))) #f)
(check-expect (json-equal? #hash((b . 2) (a . 1)) #hash((a . 1) (b . 2))) #t)
(check-expect (json-equal? #hasheq((b . 2) (a . 1)) #hash((a . 1) (b . 2))) #t)
(check-expect (json-equal? 1 "1") #f)
(check-expect (json-equal? 1 '(1)) #f)
(check-expect (json-equal? 1 #f) #f)

(check-expect (roundtrip-equal? #hasheq((a . 1) (b . 2))) #t)

(test)

#lang racket/base

(require "mapping.rkt")
(require rackunit)

(define-mapping a->b b->a
  (a b))

(check-equal? (a->b 'a) 'b)
(check-equal? (b->a 'b) 'a)
(check-exn exn:fail:contract? (lambda () (a->b 123)))
(check-exn exn:fail:contract? (lambda () (a->b 'b)))
(check-exn exn:fail:contract? (lambda () (b->a 123)))
(check-exn exn:fail:contract? (lambda () (b->a 'a)))

(define-mapping c->d d->c
  #:forward-default (lambda (x) 'default-d)
  #:backward-default (lambda (x) 'default-c)
  (c 123)
  (e 234))

(check-equal? (c->d 'c) 123)
(check-equal? (d->c 234) 'e)
(check-equal? (c->d 'other) 'default-d)
(check-equal? (d->c '235) 'default-c)

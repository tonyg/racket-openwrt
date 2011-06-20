#lang racket

(require "../../scheme-httpd/racket/httpd-utils.rkt")
(require "../../scheme-httpd/racket/httpd-servlet.rkt")
(require "../../scheme-httpd/racket/httpd.rkt")

(require "../../xxexpr/xxexpr.rkt")

(require srfi/1)
(require srfi/9)
(require srfi/13)

(define (port->lines/close p)
  (let ((v (port->lines p)))
    (close-input-port p)
    v))

(define (port->string/close p)
  (let ((v (port->string p)))
    (close-input-port p)
    v))

(include "simple-pipeline.scm")
(include "uci.scm")
(include "server.scm")

#lang racket
;;; json-unparsing: SJSON printing and pretty-printing, compatible
;;; with Neil Van Dyke's json-parsing.ss module.
;;;
;;; Copyright (C) 2011 Tony Garnock-Jones <tonyg@ccs.neu.edu>
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Lesser General Public
;;; License as published by the Free Software Foundation; either
;;; version 2.1 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301 USA

(require "json-parsing.ss") ;; for json->sjson for pretty-printing.

(provide sjson->json
	 write-sjson
	 default-json-indent
	 json-equal?
	 pretty-print-json
	 pretty-print-json-file)

(define (pretty-print-json in #:indent (indent #t))
  (display (sjson->json (json->sjson in) #:indent indent)))

(define (pretty-print-json-file filename #:indent (indent #t))
  (call-with-input-file filename
    (lambda (port)
      (pretty-print-json port #:indent indent))))

(define (sjson->json sjson #:indent (indent #f))
  (let ((p (open-output-string)))
    (write-sjson sjson p #:indent indent)
    (get-output-string p)))

(define default-json-indent 2)

(define (write-sjson sjson port #:indent (indent #f))
  (write-indented-sjson sjson
			port
			(if indent 0 #f)
			(if (number? indent) indent default-json-indent)))

(define (stringify-key sjson)
  (cond
   ((string? sjson) sjson)
   ((symbol? sjson) (symbol->string sjson))
   ((number? sjson) (number->string sjson))
   ((boolean? sjson) (if sjson "true" "false"))
   ((or (eq? sjson #\null)
	(void? sjson))
    "null")
   (else (error 'sjson->json "Cannot convert sjson value ~s to JSON dictionary key" sjson))))

(define (next-indent-level indent-level indent-delta)
  (and (number? indent-level)
       (+ indent-level indent-delta)))

(define (write-indent port indent-level)
  (when indent-level
    (newline port)
    (display (make-string indent-level #\space) port)))

(define (should-indent-table? h)
  (positive? (- (hash-count h) 1)))

(define (write-indented-sjson sjson port indent-level indent-delta)
  (cond
   ((hash? sjson)
    (display #\{ port)
    (let ((new-level (next-indent-level indent-level indent-delta))
	  (should-indent (should-indent-table? sjson)))
      (for/fold
	  ((need-comma #f))
	  (((key value) sjson))
	(when need-comma (display #\, port))
	(when should-indent
	  (write-indent port new-level))
	(write (stringify-key key) port)
	(display #\: port)
	(when indent-level
	  (display #\space port))
	(write-indented-sjson value port new-level indent-delta)
	#t)
      (when should-indent
	(write-indent port indent-level)))
    (display #\} port))
   ((list? sjson)
    (display #\[ port)
    (for/fold
	((need-comma #f))
	((value sjson))
      (when need-comma
	(display #\, port)
	(when indent-level
	  (display #\space port)))
      (write-indented-sjson value port indent-level indent-delta)
      #t)
    (display #\] port))
   ((string? sjson)
    (write sjson port))
   ((number? sjson)
    (display sjson port))
   ((boolean? sjson)
    (display (if sjson "true" "false") port))
   ((or (eq? sjson #\null) ; as per json-parsing.ss
	(void? sjson))
    (display "null" port))
   (else (error 'sjson->json "Cannot write sjson value ~s as json" sjson))))

(define (both? predicate a b)
  (and (predicate a)
       (predicate b)))

(define (json-equal? a b)
  (cond
   ((eq? a b) #t)
   ((both? number? a b) (= a b))
   ((both? string? a b) (string=? a b))
   ((both? hash? a b) (and (= (hash-count a) (hash-count b))
			   (call/ec
			    (lambda (escape)
			      (for (((ka va) a))
				(when (not (and (hash-has-key? b ka)
						(json-equal? va (hash-ref b ka))))
				  (escape #f)))
			      #t))))
   ((both? pair? a b) (and (json-equal? (car a) (car b))
			   (json-equal? (cdr a) (cdr b))))
   ((both? null? a b) #t)
   ((or (and (pair? a) (null? b))
	(and (null? a) (pair? b))) #f)
   (else #f)))

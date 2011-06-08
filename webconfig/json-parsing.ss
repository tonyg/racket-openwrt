#lang scheme/base
;;; @Package     json-parsing
;;; @Subtitle    JSON Parsing, Folding, and Conversion for Racket/Scheme
;;; @HomePage    http://www.neilvandyke.org/racket-json-parsing/
;;; @Author      Neil Van Dyke
;;; @Version     0.2
;;; @Date        2010-12-27
;;; @PLaneT      neil/json-parsing:1:=1

;; $Id: json-parsing.ss,v 1.41 2010/12/27 09:20:02 neilpair Exp $

;;; @legal
;;; Copyright @copyright{} 2010 Neil Van Dyke.  This program is Free Software;
;;; you can redistribute it and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software Foundation; either
;;; version 3 of the License (LGPL 3), or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful, but without
;;; any warranty; without even the implied warranty of merchantability or
;;; fitness for a particular purpose.  See
;;; @indicateurl{http://www.gnu.org/licenses/} for details.  For other licenses
;;; and consulting, please contact the author.
;;; @end legal

;; (require (for-syntax scheme/base))

;;; @section Introduction

;;; The @b{json-parsing} package for Racket provides JSON parsing and format
;;; conversion using a streaming tree fold.  This tree fold approach permits
;;; processing JSON input of arbitrary size in relatively small space for some
;;; applications, unlike the common approach of parsing the entire input to an
;;; AST before processing the AST.
;;;
;;; The supported JSON format is as specified on @uref{http://json.org/}, as
;;; viewed on 2010-12-25.
;;;
;;; The format converters in package include a convertor to SJSON s-expression
;;; format.  SJSON has been made to be fully compatible with the @i{jsexpr} of
;;; Dave Herman's PLaneT package @code{dherman/json:3:=0}.
;;;
;;; The parser does not consume any characters not belonging to the JSON value,
;;; and can be used to read multiple JSON values or to be intermixed with other
;;; kinds of reading from the same input.
;;;
;;; The tree fold approach of this package's parser was inspired and informed
;;; by Oleg Kiselyov's @uref{http://okmij.org/ftp/Scheme/xml.html,SSAX} XML
;;; parsing work.
;;;
;;; Implementing the @b{json-parsing} package was originally intended as an
;;; exercise for getting more experience with SSAX-like folding, before
;;; undertaking some new XML packages, but the JSON work has turned out useful
;;; in its own right.  A future version of this package might also implement
;;; alternative tree fold approaches.

;;; @section Exceptions

;;; When the parser encounters invalid JSON, it raises an
;;; @code{exn:fail:invalid-json} exception.  While this exception will be
;;; caught by handlers such as @code{exn:fail?}, the distinct exception type
;;; permits JSON-parsing errors to be handled separately from other errors, and
;;; it also includes some location information.

;;; @defvar exn:fail:invalid-json? exn
;;;
;;; Type predicate.

;;; @defproc exn:fail:invalid-json-location exn:fail:invalid-json
;;;
;;; Gets information on the location of the error within the input stream.
;;; Currently, this is a list of three elements, of the three values returned
;;; by Racket's @code{port-next-location} procedure.

(define-struct (exn:fail:invalid-json exn:fail)
  (location)
  #:transparent)

(define (make-invalid-json-exc
         sym
         #:text               text
         #:continuation-marks continuation-marks
         #:location           (location #f))
  (let ((location (if (input-port? location)
                      (call-with-values
                          (lambda () (port-next-location location))
                        list)
                      location)))
    (make-exn:fail:invalid-json (format "~A: invalid JSON: ~A~A"
                                        sym
                                        text
                                        (if location
                                            (format ", location ~S" location)
                                            ""))
                                continuation-marks
                                location)))

(define-syntax raise-invalid-json-error
  (syntax-rules ()
    ((_ SYM ARGn ...)
     (raise (make-invalid-json-exc
             SYM
             #:continuation-marks (current-continuation-marks)
             ARGn ...)))))

;;; @section Parse Fold

(define-syntax %json-parsing:syntax-error
  (syntax-rules ()
    ((_) #f)))

(define-syntax %json-parsing:case-read-token
  (syntax-rules ()
    ((_ (IN ...) SEED C0 Cn ...)
     (let ((in (IN ...)))
       (%json-parsing:case-read-token in SEED C0 Cn ...)))
    ((_ IN (SEED ...) C0 Cn ...)
     (let ((seed (SEED ...)))
       (%json-parsing:case-read-token IN seed C0 Cn ...)))
    ((_ IN SEED C0 Cn ...)
     (%json-parsing:case-read-token:2 (C0 Cn ...) () IN SEED (C0 Cn ...)))))

(define-syntax %json-parsing:case-read-token:2
  ;; (_ Cs As IN SEED SCs)
  (syntax-rules (else else-error)
    ;; No more clauses so go to next transformer:
    ((_ () As IN SEED SCs)
     (%json-parsing:case-read-token:3 SCs () IN SEED As))
    ;; Else clause:
    ((_ ((else En ...) Cn ...) As IN SEED SCs)
     (%json-parsing:case-read-token:2 (Cn ...) As IN SEED SCs))
    ((_ ((else-error En ...) Cn ...) As IN SEED SCs)
     (%json-parsing:case-read-token:2 (Cn ...) As IN SEED SCs))
    ;; Clause, so add to acceptable tokens:
    ((_ ((T En ...) Cn ...) (An ...) IN SEED SCs)
     (%json-parsing:case-read-token:2 (Cn ...) (An ... T) IN SEED SCs))
    ;; Error: invalid clause.  Internal error.
    ((_ (C0 Cn ...) (An ...) IN SEED SCs)
     (%json-parsing:syntax-error "invalid case-read-token clause" C0))))

(define-syntax %json-parsing:case-read-token:3
  ;; (_ ICs OCs IN SEED As)
  (syntax-rules (=>
                 open-curly
                 close-curly
                 comma
                 colon
                 open-square
                 close-square
                 string
                 number
                 true
                 false
                 null
                 else)
    ;; No clauses left, so assemble.
    ((_ () (OCn ...) IN SEED As)
     (let loop ()
       (let ((c (peek-char IN)))
         (case c
           ((#\space #\tab #\return #\newline #\page) (read-char IN) (loop))
           OCn ...))))
    ;; Clauses...
    ((_ ((open-curly E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3 (ICn ...)
                                      (OCn ... ((#\{) (read-char IN) E0 En ...))
                                      IN SEED As))
    ((_ ((close-curly E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3 (ICn ...)
                                      (OCn ... ((#\}) (read-char IN) E0 En ...))
                                      IN SEED As))
    ((_ ((comma E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3 (ICn ...)
                                      (OCn ... ((#\,) (read-char IN) E0 En ...))
                                      IN SEED As))
    ((_ ((colon E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3 (ICn ...)
                                      (OCn ... ((#\:) (read-char IN) E0 En ...))
                                      IN SEED As))
    ((_ ((open-square E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3 (ICn ...)
                                      (OCn ... ((#\[) (read-char IN) E0 En ...))
                                      IN SEED As))
    ((_ ((close-square E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3 (ICn ...)
                                      (OCn ... ((#\]) (read-char IN) E0 En ...))
                                      IN SEED As))
    ((_ ((string => P) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3
      (ICn ...)
      (OCn ...
           ((#\")
            (read-char IN)
            (P (%json-parsing:read-string IN) SEED)))
      IN SEED As))
    ((_ ((number => P) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3
      (ICn ...)
      (OCn ... ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)
                (P (%json-parsing:read-number IN) SEED)))
      IN SEED As))
    ((_ ((true => P) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3
      (ICn ...)
      (OCn ... ((#\t)
                (%json-parsing:parse-keyword
                 IN (#\r #\u #\e) "true" ((P 'true SEED)))))
      IN SEED As))
    ((_ ((false => P) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3
      (ICn ...)
      (OCn ... ((#\f)
                (%json-parsing:parse-keyword
                 IN (#\a #\l #\s #\e) "false" ((P 'false SEED)))))
      IN SEED As))
    ((_ ((null => P) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3
      (ICn ...)
      (OCn ... ((#\n)
                (%json-parsing:parse-keyword
                 IN (#\u #\l #\l) "null" ((P 'null SEED)))))
      IN SEED As))
    ((_ ((else E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3 (ICn ...)
                                      (OCn ... (else E0 En ...))
                                      IN SEED As))
    ((_ ((else X ...) IC0 ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:syntax-error "stuff after else clause" (else X ...)))
    ((_ ((else-error CONTEXT) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:case-read-token:3
      (ICn ...)
      (OCn ... (else (raise-invalid-json-error
                      '%<json-parsing:case-read-token>
                      #:text (format "character ~S in context ~S"
                                     (peek-char IN)
                                     CONTEXT)
                      #:location IN)))
      IN SEED As))
    ((_ ((else-error X ...) IC0 ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:syntax-error "stuff after else-error clause"
                                 (else-error X ...)))
    ((_ ((T E0 En ...) ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:syntax-error
      "invalid token name" T "in clause" (T E0 En ...)))
    ((_ (IC0 ICn ...) (OCn ...) IN SEED As)
     (%json-parsing:syntax-error "invalid clause" IC0))))

(define-syntax %json-parsing:parse-keyword
  (syntax-rules ()
    ((_ IN (Cn ...) KW (E0 En ...))
     (begin (read-char IN)
            (if (and (eqv? (read-char IN) Cn) ...
                     (let ((c (peek-char IN)))
                       (or (eof-object? c)
                           ;; TODO: Rework this test to use "char-alphabetic?"
                           ;; and perhaps check for other chars too.
                           (let ((n (char->integer c)))
                             (not (or (<= 97 n 122)
                                      (<= 65 n 90)
                                      (<= 48 n 57)))))))
                (begin E0 En ...)
                (raise-invalid-json-error
                 '%json-parsing:parse-keyword
                 #:text (format "invalid keyword that started to be ~S" KW)
                 #:location IN))))))

(define (%json-parsing:read-string in)
  (let loop ((result '()))
    (let ((c (read-char in)))
      (case c
        ((#\") (apply string (reverse result)))
        ((#\\)
         (let ((c (read-char in)))
           (case c
             ((#\" #\\ #\/) (loop (cons c           result)))
             ((#\b)         (loop (cons #\backspace result)))
             ((#\f)         (loop (cons #\page      result)))
             ((#\n)         (loop (cons #\newline   result)))
             ((#\r)         (loop (cons #\return    result)))
             ((#\t)         (loop (cons #\tab       result)))
             ((#\u)
              (let loop-u ((mults  '(4096 256 16 1))
                           (num 0))
                (if (null? mults)
                    (loop (cons (integer->char num) result))
                    ;; TODO: Maybe make this tail calls.  Or otherwise do it
                    ;; faster.
                    (loop-u (cdr mults)
                            (+ num
                               (* (car mults)
                                  (let ((c (read-char in)))
                                    (case c
                                      ((#\0)     0)
                                      ((#\1)     1)
                                      ((#\2)     2)
                                      ((#\3)     3)
                                      ((#\4)     4)
                                      ((#\5)     5)
                                      ((#\6)     6)
                                      ((#\7)     7)
                                      ((#\8)     8)
                                      ((#\9)     9)
                                      ((#\a #\A) 10)
                                      ((#\b #\B) 11)
                                      ((#\c #\C) 12)
                                      ((#\d #\D) 13)
                                      ((#\e #\E) 14)
                                      ((#\f #\F) 15)
                                      (else
                                       (raise-invalid-json-error
                                        '%json-parsing:read-string
                                        #:text
                                        (format
                                         "invalid character ~S in \\u in string"
                                         c)
                                        #:location in))))))))))
             (else (raise-invalid-json-error
                    '%json-parsing:read-string
                    #:text (format "invalid escape sequence \"\\~A\" in string"
                                   c)
                    #:location in)))))
        (else (if (eof-object? c)
                  (raise-invalid-json-error '%json-parsing:read-string
                                            #:text "EOF in string"
                                            #:location in)
                  (loop (cons c result))))))))

(define %json-parsing:read-number
  ;; TODO: We could do this a little more optimally, as a DFA, and with more
  ;; tail calls.
  (letrec ((read-digits
            (lambda (in chars required?)
              ;; TODO: Maybe use "max", to prevent stupid DoS, such as sending
              ;; huge exponents that take a lot of compute time to calculate.
              (let loop ((chars     chars)
                         (required? required?))
                (let ((c (peek-char in)))
                  (case c
                    ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                     (read-char in)
                     (loop (cons c chars) #f))
                    (else (if required?
                              (raise-invalid-json-error
                               '%json-parsing:read-number
                               #:text "missing digits in number"
                               #:location in)
                              chars))))))))
    (lambda (in)
      (let* ((chars (let ((c (read-char in)))
                      (case c
                        ((#\-) (read-digits in '(#\-) #t))
                        (else  (read-digits in `(,c)  #f)))))
             (chars (let ((c (peek-char in)))
                      (case c
                        ((#\.)
                         (read-char in)
                         (read-digits in `(#\. ,@chars) #t))
                        (else  chars))))
             (chars (case (peek-char in)
                      ((#\e #\E)
                       (read-char in)
                       (case (peek-char in)
                         ((#\-)
                          (read-char in)
                          (read-digits in `(#\- #\e ,@chars) #t))
                         ((#\+)
                          (read-char in)
                          (read-digits in `(#\+ #\e ,@chars) #t))
                         (else
                          (read-digits in `(    #\e ,@chars) #t))))
                      (else chars))))
        (let ((c (peek-char in)))
          (if (or (eof-object? c)
                  (not (or (eqv? #\- c)
                           (eqv? #\+ c)
                           (char-alphabetic? c))))
              (string->number (apply string (reverse chars)))
              (raise-invalid-json-error
               '%json-parsing:read-number
               #:text (format "invalid character ~S after number" c)
               #:location in)))))))

;;; @defsyntax json-fold-lambda ...
;;;
;;; Special syntax that expands to a JSON parser procedure.  Normally you would
;;; use this if you were defining a new application of what processing the
;;; parser should do while it is parsing JSON.
;;;
;;; The resulting procedure of this syntax the arguments:
;;;
;;; @example
;;; ( in seed exhaust? )
;;; @end example
;;;
;;; where @code{in} is an input port or string, @code{seed} is a seed value,
;;; and @code{exhaust?} is whether or not to exhaustively consume all input and
;;; ensure that there is no other non-JSON-whitespace.
;;;
;;; @code{json-fold-lambda} has many arguments, all of which must be present.
;;; Here is an example of how you might define a @code{my-json-to-sjson}
;;; procedure using @code{json-fold-lambda}:
;;;
;;; @lisp
;;; (define my-json-to-sjson
;;;   (json-fold-lambda
;;;    #:error-name         'my-json-to-sjson
;;;    #:visit-object-start (lambda (seed)
;;;                           (make-hasheq))
;;;    #:visit-object-end   (lambda (seed parent-seed)
;;;                           `(,seed ,@@parent-seed))
;;;    #:visit-member-start (lambda (name seed)
;;;                           '())
;;;    #:visit-member-end   (lambda (name seed parent-seed)
;;;                           (hash-set! parent-seed
;;;                                      (string->symbol name)
;;;                                      (car seed))
;;;                           parent-seed)
;;;    #:visit-array-start  (lambda (seed)
;;;                           '())
;;;    #:visit-array-end    (lambda (seed parent-seed)
;;;                           `(,(reverse seed) ,@@parent-seed))
;;;    #:visit-string       (lambda (str seed)
;;;                           `(,str ,@@seed))
;;;    #:visit-number       (lambda (num seed)
;;;                           `(,num ,@@seed))
;;;    #:visit-constant     (lambda (name seed)
;;;                           `(,(case name
;;;                                ((true)  #t)
;;;                                ((false) #f)
;;;                                ((null)  #\null)
;;;                                (else (error 'my-json-to-sjson
;;;                                             "invalid constant ~S"
;;;                                             name)))
;;;                             ,@@seed))))
;;; @end lisp
;;;
;;; As you can see, the arguments provide a set of procedures that are applied
;;; at various states in the parsing.  Each of these callback procedures
;;; accepts at least one seed value from its preceding sibling and/or parent,
;;; and it produces a seed value for the next sibling, child, or parent.
;;;
;;; The concepts @i{object}, @i{member}, and @i{array} are non-leaf notes in
;;; the tree.  The @i{start} callback for each non-leaf node receives a seed
;;; from its preceding sibling, and the value it produces is the seed for its
;;; first child.  The @i{end} callback receives both the seed from the last
;;; child, and the parent seed (the sibling predecessor seed of the node; the
;;; same seed received by the corresponding @i{start}).
;;;
;;; The leaf nodes each simply receive a seed from the sibling predecessor
;;; callback (or, if the first sibling, from the parent @i{start}; or, if the
;;; first callback, from the seed provided to the parser call), and provide one
;;; to the sibling successor (or, if the last sibling, to the parent @i{end};
;;; or, if the last callback, to the result of the parser call).
;;;
;;; Note that two different techniques are used above to build collections of
;;; objects during processing, using seeds.  The first is to use a hash that is
;;; passed in the seed, which in this case is used because SJSON requires a
;;; hash as part of its format.  The second, and more common, is to construct
;;; lists by incrementally consing onto the front of the list, so that the list
;;; is ordred backwards, and waiting til the list is finished to put it in
;;; correct order using the @code{reverse} procedure.
;;;
;;; The parser procedure returns either the value of the last callback, or, if
;;; the end of the input is reached without a JSON value, the @code{eof}
;;; object.

(define-syntax json-fold-lambda
  (syntax-rules ()
    ((_ #:error-name         EN
        #:visit-object-start VOS
        #:visit-object-end   VOE
        #:visit-member-start VMS
        #:visit-member-end   VME
        #:visit-array-start  VAS
        #:visit-array-end    VAE
        #:visit-string       VS
        #:visit-number       VN
        #:visit-constant     VC)
     (letrec
         (
          ;; Begin Macro Arguments.
          (error-name         EN)
          (visit-object-start VOS)
          (visit-object-end   VOE)
          (visit-member-start VMS)
          (visit-member-end   VME)
          (visit-array-start  VAS)
          (visit-array-end    VAE)
          (visit-string       VS)
          (visit-number       VN)
          (visit-constant     VC)
          ;; End Macro Arguments.
          (do-value
           (lambda (in seed)
             (%json-parsing:case-read-token
              in
              seed
              (open-curly  (do-object in seed))
              (open-square (do-array  in seed))
              (string      => visit-string)
              (number      => visit-number)
              (true        => visit-constant)
              (false       => visit-constant)
              (null        => visit-constant)
              (else-error  "value"))))
          (do-object
           (lambda (in seed)
             (do-object-members in (visit-object-start seed) seed)))
          (do-object-members
           (lambda (in object-seed object-parent-seed)
             (%json-parsing:case-read-token
              in
              'dummy-seed
              (close-curly (visit-object-end object-seed object-parent-seed))
              (string
               => (lambda (name dummy-seed)
                    (let ((value-seed (visit-member-start name object-seed)))
                      (%json-parsing:case-read-token
                       in
                       value-seed
                       (colon
                        (let ((value-seed (do-value in value-seed)))
                          (let ((object-seed (visit-member-end
                                              name
                                              value-seed object-seed)))
                            (%json-parsing:case-read-token
                             in
                             object-seed
                             (close-curly (visit-object-end
                                           object-seed
                                           object-parent-seed))
                             (comma       (do-object-members
                                           in
                                           object-seed
                                           object-parent-seed))
                             (else-error  "object comma or end")))))
                       (else-error "object member colon")))))
              (else-error  "object member name or end"))))
          (do-array
           (lambda (in seed)
             (do-array-members in (visit-array-start seed) seed)))
          (do-array-members
           (lambda (in seed parent-seed)
             ;; TODO: We're doing an extra dispatch on char with the "else"
             ;; clause going to "do-value" here.
             (%json-parsing:case-read-token
              in
              seed
              (close-square (visit-array-end seed parent-seed))
              (else         (let ((seed (do-value in seed)))
                              (%json-parsing:case-read-token
                               in
                               seed
                               (close-square (visit-array-end seed
                                                              parent-seed))
                               (comma        (do-array-members in
                                                               seed
                                                               parent-seed))
                               (else-error   "array comma or end"))))))))
       ;; TODO: Add an option for exhausting the input.
       (lambda (in seed exhaust?)
         (let ((in (if (string? in)
                       (open-input-string in)
                       in)))
           ;; Use our token macro to skip over whitespace and check for EOF,
           ;; then either return EOF or parse a value.
           (%json-parsing:case-read-token
            in
            #f
            (else
             (let ((c (peek-char in)))
               (if (eof-object? c)
                   c
                   (begin0 (do-value in seed)
                     (and exhaust?
                          (%json-parsing:case-read-token
                           in
                           #f
                           (else
                            (or (eof-object? (peek-char in))
                                (raise-invalid-json-error
                                 error-name
                                 #:text
                                 (format "input not exhausted; character ~S"
                                         (peek-char in))
                                 #:location in))))))))))))))))

;;; @defproc make-json-fold ...
;;;
;;; This is like @code{json-fold-lambda}, except it is a procedure, rather than
;;; syntax.  @code{make-json-fold} can be used in the less-common case that you
;;; need to define a new parser dynamically.
;;;
;;; Note that, in the produced procedure, the @code{exhaust?} argument is
;;; optional (defaulting to @code{#t}).  Thus, the signature is:
;;;
;;; @example
;;; ( in seed @{ #:exhaust? exhaust? @}? )
;;; @end example

(define (make-json-fold
         #:error-name         (error-name '<make-json-fold>)
         #:visit-object-start visit-object-start
         #:visit-object-end   visit-object-end
         #:visit-member-start visit-member-start
         #:visit-member-end   visit-member-end
         #:visit-array-start  visit-array-start
         #:visit-array-end    visit-array-end
         #:visit-string       visit-string
         #:visit-number       visit-number
         #:visit-constant     visit-constant)
  (json-fold-lambda
   #:error-name         error-name
   #:visit-object-start visit-object-start
   #:visit-object-end   visit-object-end
   #:visit-member-start visit-member-start
   #:visit-member-end   visit-member-end
   #:visit-array-start  visit-array-start
   #:visit-array-end    visit-array-end
   #:visit-string       visit-string
   #:visit-number       visit-number
   #:visit-constant     visit-constant))

;;; @section Conversion

;;; @defproc json->sjson in [ #:exhaust? exhaust? ]
;;;
;;; Parse a JSON value from input port or string @var{in}, and return an SJSON
;;; parsed representation.  SJSON is identical to the @i{jsexpr} defined by the
;;; PLaneT package @code{dherman/json:3:=0}.

;; TODO: Make the hashes immutable (build an alist while parsing, and convert
;; it with "make-immutable-hasheq" as finishing)?

(define %json-parsing:json->sjson:fold
  (json-fold-lambda
   #:error-name         'json->sjson
   #:visit-object-start (lambda (seed)
                          (make-hasheq))
   #:visit-object-end   (lambda (seed parent-seed)
                          `(,seed ,@parent-seed))
   #:visit-member-start (lambda (name seed)
                          '())
   #:visit-member-end   (lambda (name seed parent-seed)
                          (hash-set! parent-seed
                                     (string->symbol name)
                                     (car seed))
                          parent-seed)
   #:visit-array-start  (lambda (seed)
                          '())
   #:visit-array-end    (lambda (seed parent-seed)
                          `(,(reverse seed) ,@parent-seed))
   #:visit-string       (lambda (str seed)
                          `(,str ,@seed))
   #:visit-number       (lambda (num seed)
                          `(,num ,@seed))
   #:visit-constant     (lambda (name seed)
                          `(,(case name
                               ((true)  #t)
                               ((false) #f)
                               ((null)  #\null)
                               (else (error 'json->sjson
                                            "invalid constant ~S"
                                            name)))
                            ,@seed))))

(define (json->sjson in #:exhaust? (exhaust? #t))
  (let ((result (%json-parsing:json->sjson:fold in '() exhaust?)))
    (if (eof-object? result)
        result
        (car result))))

;;; @defproc json->sxml in [ #:exhaust? exhaust? ]
;;;
;;; Parse the JSON input from input port or string @var{in}, and return in a
;;; contrived XML data format that can be processed with various SXML tools.

(define %json-parsing:json->sxml:fold
  (json-fold-lambda
   #:error-name         'json->sxml
   #:visit-object-start (lambda (seed)
                          '())
   #:visit-object-end   (lambda (seed parent-seed)
                          `((object ,@(reverse seed)) ,@parent-seed))
   #:visit-member-start (lambda (name seed)
                          '())
   #:visit-member-end   (lambda (name seed parent-seed)
                          `((member (@ (name ,name)) ,@seed) ,@parent-seed))
   #:visit-array-start  (lambda (seed)
                          '())
   #:visit-array-end    (lambda (seed parent-seed)
                          `((array ,@(reverse seed)) ,@parent-seed))
   #:visit-string       (lambda (str seed)
                          `((string ,str) ,@seed))
   #:visit-number       (lambda (num seed)
                          `((number ,(number->string num)) ,@seed))
   #:visit-constant     (lambda (name seed)
                          `((,name) ,@seed))))

(define (json->sxml in #:exhaust? (exhaust? #t))
  (let ((result (%json-parsing:json->sxml:fold in '() exhaust?)))
    (if (eof-object? result)
        result
        (cons '*TOP* result))))

;;; @defproc write-json-as-xml in [ #:exhaust? exhaust? ] [ #:out out ] 
;;;
;;; Parse the JSON input from input port or string @var{in}, and write it in
;;; contrived XML data format to output port @var{out} (which defaults to the
;;; value of the @code{current-output-port} parameter).  This is mainly a
;;; demonstration of ``streaming'' processing that can scale to arbitrary JSON
;;; input sizes.

(define %json-parsing:write-json-as-xml:fold
  ;; TODO: This doesn't properly escape special characters in attribute of
  ;; "member" element and in content of "string" element.
  ;;
  ;; TODO: Maybe add an extra fields argument to json-fold-lambda, so that
  ;; we can pass "out" directly to it?
  (lambda (in seed exhaust? out)
    ((json-fold-lambda
      #:error-name         'write-json-as-xml
      #:visit-object-start (lambda (seed)
                             (display "<object>" out)
                             #t)
      #:visit-object-end   (lambda (seed parent-seed)
                             (display "</object>" out)
                             #t)
      #:visit-member-start (lambda (name seed)
                             (fprintf out "<member name=\"~A\">" name)
                             #t)
      #:visit-member-end   (lambda (name seed parent-seed)
                             (display "</member>" out)
                             #t)
      #:visit-array-start  (lambda (seed)
                             (display "<array>" out)
                             #t)
      #:visit-array-end    (lambda (seed parent-seed)
                             (display "</array>" out)
                             #t)
      #:visit-string       (lambda (str seed)
                             (fprintf out "<string>~A</string>" str)
                             #t)
      #:visit-number       (lambda (num seed)
                             (fprintf out
                                      "<number>~A</number>"
                                      (number->string num))
                             #t)
      #:visit-constant     (lambda (name seed)
                             (fprintf out "<~A/>" name)))
     in seed exhaust?)))

(define (write-json-as-xml in
                           #:exhaust? (exhaust? #t)
                           #:out      (out      (current-output-port)))
  (or (%json-parsing:write-json-as-xml:fold in #f exhaust? out)
      (error 'write-json-as-xml
             "no JSON to read"))
  (void))

;;; @defproc json->xml in [ #:exhaust? exhaust? ]
;;;
;;; This is like @code{write-json-as-xml}, but instead of writing to a port, it
;;; returns the XML as a string.  Most people would not choose to do this.

(define (json->xml in #:exhaust? (exhaust? #t))
  (let ((out (open-output-string)))
    (write-json-as-xml in #:exhaust? exhaust? #:out out)
    (get-output-string out)))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.2 --- 2010-12-27 - PLaneT @code{(1 1)}
;;; Added missing export.
;;;
;;; @item Version 0.1 --- 2010-12-26 - PLaneT @code{(1 0)}
;;; Initial release.
;;;
;;; @end table

;; TODO: Debugging help.
;;
;; (define-syntax %helper
;;   (syntax-rules ()
;;     ((_ X ...) (lambda args (vector 'hello)))))
;; (define-syntax dummy
;;   (syntax-rules ()
;;     ((_ X ...) (list (%helper X ...)))))

(provide
 exn:fail:invalid-json-location
 exn:fail:invalid-json?
 json->sjson
 json->sxml
 json->xml
 json-fold-lambda
 make-json-fold
 write-json-as-xml)

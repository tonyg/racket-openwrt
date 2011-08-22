#lang typed/racket/base

(require (planet tonyg/bitsyntax))
(require racket/udp)
(require racket/match)

(require "mapping.rkt")

;; Protocol data taken from RFC-1035. (See also RFC-1034.)
;; Blocks of text inside <rfc1035>...</rfc1035> also from RFC-1035.
;; RFC-3596 specifies "DNS Extensions to Support IP Version 6".

;; RFC-2782 specifies the DNS SRV record, though weirdly it omits a
;; wire-level definition of the format! Presumably people have just
;; copied what they see everyone else do here!

(provide (struct-out dns-message)
	 (struct-out question)
	 (struct-out rr)
	 (struct-out hinfo)
	 (struct-out minfo)
	 (struct-out mx)
	 (struct-out soa)
	 (struct-out wks)
	 (struct-out srv)

	 value->query-opcode query-opcode->value
	 value->query-response-code query-response-code->value
	 type->value value->type
	 qtype->value value->qtype
	 class->value value->class
	 qclass->value value->qclass

	 packet->dns-message
	 dns-message->packet

	 make-dns-query
	 make-dns-response

	 raw-dns-query)

;;---------------------------------------------------------------------------
;; Data definitions

;; An unsigned four-bit quantity.
(define-type Nibble (U 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

;; Represents a domain name. The head of the list is the leftmost
;; label; for example, www.google.com is represented as '(#"www"
;; #"google" #"com").
(define-type DomainName (Listof Bytes))

;; Represents an IPv4 address. For example, 127.0.0.1 is represented
;; as (vector 127 0 0 1).
(define-type IPv4 (Vector Byte Byte Byte Byte))

;; Represents an IPv6 address. For example,
;; 2001:0db8:85a3:0000:0000:8a2e:0370:7334 is represented as (vector
;; #x20 #x01 #x0d #xb8 #x85 #xa3 #x00 #x00 #x00 #x00 #x8a #x2e #x03
;; #x70 #x73 #x34).
(define-type IPv6 (Vector Byte Byte Byte Byte
			  Byte Byte Byte Byte
			  Byte Byte Byte Byte
			  Byte Byte Byte Byte))

;; Interpreted as either a DNS request or reply, depending on the
;; Direction.
(struct: dns-message ([id : Index] ;; want this to be 16 bits wide ideally
		      [direction : (U 'request 'response)]
		      [opcode : QueryOpcode]
		      [authoritative : (U 'authoritative 'non-authoritative)]
		      [truncated : (U 'truncated 'not-truncated)]
		      [recursion-desired : (U 'recursion-desired 'no-recursion-desired)]
		      [recursion-available : (U 'recursion-available 'no-recursion-available)]
		      [response-code : ResponseCode]
		      [questions : (Listof question)]
		      [answers : (Listof rr)]
		      [authorities : (Listof rr)]
		      [additional : (Listof rr)])
	 #:transparent)

;; Represents a DNS question: "What are the RRs for the given name,
;; type and class?"
(struct: question ([name : DomainName]
		   [type : QueryType]
		   [class : QueryClass]) #:transparent)

;; Represents a resource record.
(struct: rr ([name : DomainName]
	     [type : RRType]
	     [class : RRClass]
	     [ttl : Exact-Nonnegative-Integer]
	     [rdata : RData]) #:transparent)

(define-type RData (U IPv4 ;; an "A" record
		      IPv6 ;; an "AAAA" record
		      hinfo ;; a host information record [O]
		      mx ;; a mail exchanger record
		      soa ;; a start-of-authority record
		      wks ;; a Well-Known Service [O]
		      srv ;; Uint16 Uint16 Uint16 DomainName), an "SRV" record
;;
;; In each case, the RData's variant MUST line up correctly with the
;; type field of any RR containing it.
;;
;; Many of these variants are obsolete in today's DNS database (marked
;; [O] above).
(struct: hinfo ([cpu : String] [os : String]) #:transparent)
(struct: minfo ([rmailbx : DomainName] [emailbx : DomainName]) #:transparent)
(struct: mx ([preference : Index] ;; actually 16 bits wide
	     [exchange : DomainName]) #:transparent)
(struct: soa ([mname : DomainName]
	      [rname : DomainName]
	      [serial : Exact-Nonnegative-Integer] ;; 32-bits, as are the remainder
	      [refresh : Exact-Nonnegative-Integer]
	      [retry : Exact-Nonnegative-Integer]
	      [expire : Exact-Nonnegative-Integer]
	      [minimum : Exact-Nonnegative-Integer]) #:transparent)
(struct: wks ([address : IPv4]
	      [protocol : Byte]
	      [bitmap : Bytes]) #:transparent)
(struct: srv ([priority : Index] ;; actually 16 bits wide, as are the other Indexes
	      [weight : Index]
	      [port : Index]
	      [target : DomainName]) #:transparent)

;; A QueryOpcode is a Symbol or a Number, one of the possibilities
;; given in the following define-mapping. It represents a DNS message
;; operation; see the RFC for details.
(define-type QueryOpcode (U Nibble 'query 'iquery 'status))
(define-mapping query-opcode->value value->query-opcode
  #:forward-default values
  #:backward-default values
  (query 0)
  (iquery 1)
  (status 2))

;; A ResponseCode is a Symbol or a Number, one of the possibilities
;; given in the following define-mapping. It represents the outcome of
;; a DNS query.
(define-type ResponseCode (U Nibble 'no-error 'format-error 'server-failure 
(define-mapping query-response-code->value value->query-response-code
  (no-error 0)
  (format-error 1)
  (server-failure 2)
  (name-error 3)
  (not-implemented 4)
  (refused 5))

;; An RRType is a Symbol or a Number, one of the possibilities given
;; in the following define-mapping. It represents the type of an
;; RR. When used in an RR with an RData, the RRType and the RData
;; variant must correspond.
(define-mapping type->value value->type
  #:forward-default values
  #:backward-default values
  (a 1)
  (ns 2)
  (md 3)
  (mf 4)
  (cname 5)
  (soa 6)
  (mb 7)
  (mg 8)
  (mr 9)
  (null 10)
  (wks 11)
  (ptr 12)
  (hinfo 13)
  (minfo 14)
  (mx 15)
  (txt 16)
  (aaaa 28)
  (srv 33))

;; A QueryType is a Symbol or Number (as given in the following
;; define-mapping) or an RRType. It specifies the kinds of records
;; being sought after in a DNS query.
(define-mapping qtype->value value->qtype
  #:forward-default type->value
  #:backward-default value->type
  (axfr 252)
  (mailb 253)
  (maila 254)
  (* 255))

;; An RRClass is a Symbol or a Number, one of the possibilities given
;; in the following define-mapping. It represents the "class" of DNS
;; records being discussed. All classes except 'in are obsolete in
;; today's DNS databases.
(define-mapping class->value value->class
  #:forward-default values
  #:backward-default values
  (in 1)
  (cs 2)
  (ch 3)
  (hs 4))

;; A QueryClass is a Symbol or Number (as given in the following
;; define-mapping) or an RRClass. It specifies the "class" of records
;; being sought after in a DNS query.
(define-mapping qclass->value value->qclass
  #:forward-default class->value
  #:backward-default value->class
  (* 255))

;;---------------------------------------------------------------------------
;; DNS message codec

;; <rfc1035>
;; All communications inside of the domain protocol are carried in a single
;; format called a message.  The top level format of message is divided
;; into 5 sections (some of which are empty in certain cases) shown below:
;;
;;     +---------------------+
;;     |        Header       |
;;     +---------------------+
;;     |       Question      | the question for the name server
;;     +---------------------+
;;     |        Answer       | RRs answering the question
;;     +---------------------+
;;     |      Authority      | RRs pointing toward an authority
;;     +---------------------+
;;     |      Additional     | RRs holding additional information
;;     +---------------------+
;; </rfc1035>

;; <rfc1035>
;; The header contains the following fields:
;;
;;                                     1  1  1  1  1  1
;;       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                      ID                       |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    QDCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    ANCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    NSCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                    ARCOUNT                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;; </rfc1035>

(define (bit->value n if0 if1)
  (if (positive? n) if1 if0))

(define (value->bit b if0 if1)
  (cond
   ((eq? b if0) 0)
   ((eq? b if1) 1)
   (else (error 'value->bit "Value supplied is neither ~v nor ~v: ~v" if0 if1 b))))

(define (packet->dns-message packet)
  (bit-string-case packet
    ([ (id :: bits 16)
       (qr :: bits 1)
       (opcode :: bits 4)
       (aa :: bits 1)
       (tc :: bits 1)
       (rd :: bits 1)
       (ra :: bits 1)
       (= 0 :: bits 3)
       (rcode :: bits 4)
       (qdcount :: bits 16)
       (ancount :: bits 16)
       (nscount :: bits 16)
       (arcount :: bits 16)
       (sections4 :: binary) ]
     (let*-values (((q-section sections3)
		    (parse-section packet decode-question qdcount sections4))
		   ((a-section sections2)
		    (parse-section packet decode-rr ancount sections3))
		   ((auth-section sections1)
		    (parse-section packet decode-rr nscount sections2))
		   ((additional-section sections0)
		    (parse-section packet decode-rr arcount sections1)))
       (when (not (zero? (bit-string-length sections0)))
	 (error 'packet->dns-message "Packet too long"))
       (dns-message id
		    (bit->value qr 'request 'response)
		    (value->query-opcode opcode)
		    (bit->value aa 'non-authoritative 'authoritative)
		    (bit->value tc 'not-truncated 'truncated)
		    (bit->value rd 'no-recursion-desired 'recursion-desired)
		    (bit->value ra 'no-recursion-available 'recursion-available)
		    (value->query-response-code rcode)
		    q-section
		    a-section
		    auth-section
		    additional-section)))))

(define (dns-message->packet m)
  (bit-string->bytes
   (bit-string ((dns-message-id m) :: bits 16)
	       ((value->bit (dns-message-direction m)
			    'request 'response) :: bits 1)
	       ((query-opcode->value (dns-message-opcode m)) :: bits 4)
	       ((value->bit (dns-message-authoritative m)
			    'non-authoritative 'authoritative) :: bits 1)
	       ((value->bit (dns-message-truncated m)
			    'not-truncated 'truncated) :: bits 1)
	       ((value->bit (dns-message-recursion-desired m)
			    'no-recursion-desired 'recursion-desired) :: bits 1)
	       ((value->bit (dns-message-recursion-available m)
			    'no-recursion-available 'recursion-available) :: bits 1)
	       (0 :: bits 3)
	       ((query-response-code->value (dns-message-response-code m)) :: bits 4)
	       ((length (dns-message-questions m)) :: bits 16)
	       ((length (dns-message-answers m)) :: bits 16)
	       ((length (dns-message-authorities m)) :: bits 16)
	       ((length (dns-message-additional m)) :: bits 16)
	       ((bit-string-append
		 (encode-section encode-question (dns-message-questions m))
		 (encode-section encode-rr (dns-message-answers m))
		 (encode-section encode-rr (dns-message-authorities m))
		 (encode-section encode-rr (dns-message-additional m))) :: binary))))

(define (parse-section packet parser remaining-records input)
  (let loop ((count remaining-records)
	     (input input))
    (cond
     ((positive? count)
      (let*-values (((record remainder) (parser packet input))
		    ((records final-remainder) (loop (sub1 count) remainder)))
	(values (cons record records) final-remainder)))
     (else
      (values '() input)))))

(define (encode-section encoder records)
  (cond
   ((null? records) (bytes))
   ((null? (cdr records)) (encoder (car records)))
   (else (bit-string-append (encoder (car records))
			    (encode-section encoder (cdr records))))))

;; Domain-names use a strange "compressed" encoding.
;; We have to be careful not to get stuck in a pointer loop here.

(define (parse-domain-name whole-packet input pointers-followed)
  (bit-string-case input

    ([(= 3 :: bits 2) (offset :: bits 14) (rest :: binary)]
     (if (member offset pointers-followed)
	 (error 'parse-domain-name "DNS compressed-pointer loop detected")
	 (let-values (((lhs rhs) (bit-string-split-at whole-packet (* 8 offset))))
	   (let-values (((labels ignored-tail)
			 (parse-domain-name whole-packet rhs (cons offset pointers-followed))))
	     (values labels rest)))))

    ([(= 0 :: bits 8) (rest :: binary)]
     (values '() rest))

    ([(= 0 :: bits 2) (len :: bits 6) (label :: binary bytes len) (rest :: binary)]
     ;; TODO: validate labels: make sure they conform to the prescribed syntax
     (let-values (((labels leftover)
		   (parse-domain-name whole-packet rest pointers-followed)))
       (values (cons (bit-string->bytes label) labels) leftover)))))

(define (parse-single-domain-name whole-packet input)
  (let-values (((name remainder) (parse-domain-name whole-packet input '())))
    (if (bit-string-empty? remainder)
	name
	(error 'parse-single-domain-name
	       "Expected just the one name, but got some trailing junk"))))

(define (extract-domain-names whole-packet input)
  (if (bit-string-empty? input)
      (let-values (((name remainder) (parse-domain-name whole-packet input '())))
	(cons name (extract-domain-names whole-packet remainder)))
      '()))

(define (encode-domain-name labels)
  (cond
   ((null? labels) (bytes 0))
   (else (bit-string-append (encode-label (car labels))
			    (encode-domain-name (cdr labels))))))

(define (encode-label label)
  (encode-pascal-string "Label" 64 label))

(define (encode-pascal-string string-kind length-limit s)
  (let ((len (bytes-length s)))
    (when (>= len length-limit)
      (error 'encode-pascal-string "~s too long: ~v" string-kind s))
    (bytes-append (bytes len) s)))

;; Character strings are pascal-style length-byte-prefixed strings.

(define (extract-character-strings input)
  (bit-string-case input
    ([]
     '())
    ([len (body :: binary bytes len) (rest :: binary)]
     (cons (bit-string->bytes body)
	   (extract-character-strings rest)))))

(define (encode-character-string bs)
  (encode-pascal-string "Character-string" 256 bs))

;; <rfc1035>
;; The question section is used to carry the "question" in most queries,
;; i.e., the parameters that define what is being asked.  The section
;; contains QDCOUNT (usually 1) entries, each of the following format:
;;
;;                                     1  1  1  1  1  1
;;       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                                               |
;;     /                     QNAME                     /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                     QTYPE                     |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                     QCLASS                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;; </rfc1035>

(define (decode-question whole-packet input)
  (let-values (((qname remainder) (parse-domain-name whole-packet input '())))
    (bit-string-case remainder
      ([(qtype :: bits 16)
	(qclass :: bits 16)
	(tail :: binary)]
       (values (question qname
			 (value->qtype qtype)
			 (value->qclass qclass))
	       tail)))))

(define (encode-question q)
  (bit-string-append (encode-domain-name (question-name q))
		     (bit-string ((qtype->value (question-type q)) :: bits 16)
				 ((qclass->value (question-class q)) :: bits 16))))

;; <rfc1035>
;; All RRs have the same top level format shown below:
;;
;;                                     1  1  1  1  1  1
;;       0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                                               |
;;     /                                               /
;;     /                      NAME                     /
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                      TYPE                     |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                     CLASS                     |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                      TTL                      |
;;     |                                               |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;;     |                   RDLENGTH                    |
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
;;     /                     RDATA                     /
;;     /                                               /
;;     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;; </rfc1035>

(define (decode-rr whole-packet input)
  (let-values (((name remainder) (parse-domain-name whole-packet input '())))
    (bit-string-case remainder
      ([(type-number :: bits 16)
	(class :: bits 16)
	(ttl :: bits 32)
	(rdlength :: bits 16)
	(rdata :: binary bytes rdlength)
	(tail :: binary)]
       (let ((type (value->type type-number)))
	 (values (rr name
		     type
		     (value->class class)
		     ttl
		     (decode-rdata whole-packet type rdata))
		 tail))))))

(define (decode-rdata whole-packet type rdata)
  (case type
    ((cname mb md mf mg mr ns ptr) (parse-single-domain-name whole-packet rdata))
    ((hinfo) (apply hinfo (extract-character-strings rdata)))
    ((minfo) (apply minfo (extract-domain-names whole-packet rdata)))
    ((mx) (bit-string-case rdata
	    ([(preference :: bits 16) (exchange :: binary)]
	     (mx preference (parse-single-domain-name whole-packet exchange)))))
    ((null) (bit-string->bytes rdata))
    ((soa) (let*-values (((mname rdata1) (parse-domain-name whole-packet rdata '()))
			 ((rname rdata2) (parse-domain-name whole-packet rdata1 '())))
	     (bit-string-case rdata2
	       ([(serial :: bits 32)
		 (refresh :: bits 32)
		 (retry :: bits 32)
		 (expire :: bits 32)
		 (minimum :: bits 32)]
		(soa mname rname serial refresh retry expire minimum)))))
    ((txt) (extract-character-strings rdata))
    ((a) (bit-string-case rdata
	   ([a b c d]
	    (vector a b c d))))
    ((aaaa) (bit-string-case rdata
	      ([(ipv6-addr :: binary bits 128)]
	       (list->vector (bytes->list (bit-string->bytes ipv6-addr))))))
    ((wks) (bit-string-case rdata
	     ([a b c d protocol (bitmap :: binary)]
	      (wks (vector a b c d) protocol bitmap))))
    ((srv) (bit-string-case rdata
	     ([(priority :: bits 16)
	       (weight :: bits 16)
	       (port :: bits 16)
	       (target :: binary)]
	      (srv priority weight port (parse-single-domain-name whole-packet target)))))
    (else (bit-string->bytes rdata))))

(define (encode-rr rr)
  (let ((encoded-rdata (encode-rdata (rr-type rr) (rr-rdata rr))))
    (bit-string-append (encode-domain-name (rr-name rr))
		       (bit-string ((type->value (rr-type rr)) :: bits 16)
				   ((class->value (rr-class rr)) :: bits 16)
				   ((rr-ttl rr) :: bits 32)
				   ((/ (bit-string-length encoded-rdata) 8) :: bits 16)
				   (encoded-rdata :: binary)))))

(define (encode-rdata type rdata)
  (case type
    ((cname mb md mf mg mr ns ptr) (encode-domain-name rdata))
    ((hinfo) (bit-string-append (encode-character-string (hinfo-cpu rdata))
				(encode-character-string (hinfo-os rdata))))
    ((minfo) (bit-string-append (encode-character-string (minfo-rmailbx rdata))
				(encode-character-string (minfo-emailbx rdata))))
    ((mx) (bit-string ((mx-preference rdata) :: bits 16)
		      ((encode-domain-name (mx-exchange rdata)) :: binary)))
    ((null) rdata)
    ((soa) (bit-string-append (encode-domain-name (soa-mname rdata))
			      (encode-domain-name (soa-rname rdata))
			      (bit-string ((soa-serial rdata) :: bits 32)
					  ((soa-refresh rdata) :: bits 32)
					  ((soa-retry rdata) :: bits 32)
					  ((soa-expire rdata) :: bits 32)
					  ((soa-minimum rdata) :: bits 32))))
    ((txt)
     ;; TODO: write and use bit-string-append* instead of using apply here
     (foldl (lambda (s acc) (bit-string-append acc (encode-character-string s)))
	    (car rdata)
	    (cdr rdata)))
    ((a) (match rdata ((vector a b c d) (bit-string a b c d))))
    ((aaaa) (bit-string ((list->bytes (vector->list rdata)) :: binary bits 128)))
    ((wks) (match (wks-address rdata)
	     ((vector a b c d)
	      (bit-string a b c d (wks-protocol rdata) ((wks-bitmap rdata) :: binary)))))
    ((srv) (bit-string ((srv-priority rdata) :: bits 16)
		       ((srv-weight rdata) :: bits 16)
		       ((srv-port rdata) :: bits 16)
		       ((encode-domain-name (srv-target rdata)) :: binary)))
    (else rdata)))

;;---------------------------------------------------------------------------

(define (make-dns-query questions
			[recursion-desired 'no-recursion-desired])
  (dns-message (random 65536)
	       'request
	       'query
	       'non-authoritative
	       'not-truncated
	       recursion-desired
	       'no-recursion-available
	       'no-error
	       questions
	       '()
	       '()
	       '()))

(define (make-dns-response query response-code answers authoritative
			   [recursion-available 'no-recursion-available]
			   [authorities '()]
			   [additional '()])
  (dns-message (dns-message-id query)
	       'response
	       (dns-message-opcode query)
	       authoritative
	       'not-truncated
	       (dns-message-recursion-desired query)
	       recursion-available
	       response-code
	       (dns-message-questions query)
	       answers
	       authorities
	       additional))

(define (next-timeout timeout)
  (case timeout
    ((3) 11)
    ((11) 45)
    ((45) #f)))

(define *total-port-finding-attempts* 100) ;; TODO: eliminate arbitrary 100?

(define (bind-to-random-port! s)
  (let find-a-port ((remaining-tries *total-port-finding-attempts*))
    (if (zero? remaining-tries)
	(error 'bind-to-random-port! "Could not find a free UDP port in ~v tries"
	       *total-port-finding-attempts*)
	(let ((port-number (+ 1024 (random (- 65536 1024)))))
	  (with-handlers [(exn:fail:network?
			   (lambda (e)
			     ;; Bind failure. Port in use?
			     (find-a-port (- remaining-tries 1))))]
	    (udp-bind! s #f port-number))))))

(define (raw-dns-query query [servers '("127.0.0.1")])
  (let ((s (udp-open-socket #f #f)))
    (bind-to-random-port! s)
    ;; TODO: randomize ordering of servers in list.
    (let search ((timeout 3)
		 (remaining-servers servers))
      (if (null? remaining-servers)
	  (let ((new-timeout (next-timeout timeout)))
	    (if new-timeout
		(search new-timeout servers)
		#f))
	  (let ((server (car remaining-servers)))
	    (let ((server-hostname (if (string? server) server (car server)))
		  (server-port (if (string? server) 53 (cadr server))))
	      ;;(write `(querying ,server-hostname ,server-port with timeout ,timeout)) (newline)
	      (udp-send-to s server-hostname server-port (dns-message->packet query))
	      (let ((buffer (make-bytes 512))) ;; maximum DNS reply length
		(let ((result (sync/timeout timeout (udp-receive!-evt s buffer))))
		  ;; TODO: maybe receive only specifically from the queried IP address?
		  ;;(write `(response ,result)) (newline)
		  (if result
		      (let ((reply-length (car result)))
			(packet->dns-message (sub-bit-string buffer 0 (* 8 reply-length))))
		      (search timeout (cdr remaining-servers)))))))))))

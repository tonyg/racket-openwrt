#lang racket/base

(require (planet tonyg/bitsyntax))
(require racket/udp)

(require "mapping.rkt")

;; Protocol data taken from RFC-1035. (See also RFC-1034.)
;; Blocks of text inside <rfc1035>...</rfc1035> also from RFC-1035.

;;---------------------------------------------------------------------------
;; Structure definitions

(struct dns-message (id
		     response?
		     opcode
		     authoritative?
		     truncated?
		     recursion-desired?
		     recursion-available?
		     response-code
		     questions
		     answers
		     authorities
		     additional)
	#:transparent)

(struct question (name type class) #:transparent)

(struct rr (name type class ttl rdata) #:transparent)

(struct hinfo (cpu os) #:transparent)

(struct minfo (rmailbx emailbx) #:transparent)

(struct mx (preference exchange) #:transparent)

(struct soa (mname rname serial refresh retry expire minimum) #:transparent)

(struct wks (address protocol bitmap) #:transparent)

;;---------------------------------------------------------------------------
;; Mappings for protocol constants of various types

(define-mapping value->query-opcode query-opcode->value
  #:forward-default values
  #:backward-default values
  (0 query)
  (1 iquery)
  (2 status))

(define-mapping value->query-response-code query-response-code->value
  (0 ok)
  (1 format-error)
  (2 server-failure)
  (3 name-error)
  (4 not-implemented)
  (5 refused))

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
  (txt 16))

(define-mapping qtype->value value->qtype
  #:forward-default type->value
  #:backward-default value->type
  (axfr 252)
  (mailb 253)
  (maila 254)
  (* 255))

(define-mapping class->value value->class
  #:forward-default values
  #:backward-default values
  (in 1)
  (cs 2)
  (ch 3)
  (hs 4))

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

(define (bit->boolean n)
  (positive? n))

(define (boolean->bit b)
  (if b 1 0))

(define (packet->dns-message packet)
  (bit-string-case packet
    ([ (id : bits 16)
       (qr : bits 1)
       (opcode : bits 4)
       (aa : bits 1)
       (tc : bits 1)
       (rd : bits 1)
       (ra : bits 1)
       (= 0 : bits 3)
       (rcode : bits 4)
       (qdcount : bits 16)
       (ancount : bits 16)
       (nscount : bits 16)
       (arcount : bits 16)
       (sections4 : binary) ]
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
		    (bit->boolean qr)
		    (value->query-opcode opcode)
		    (bit->boolean aa)
		    (bit->boolean tc)
		    (bit->boolean rd)
		    (bit->boolean ra)
		    (value->query-response-code rcode)
		    q-section
		    a-section
		    auth-section
		    additional-section)))))

(define (dns-message->packet m)
  (bit-string ((dns-message-id m) : bits 16)
	      ((boolean->bit (dns-message-response? m)) : bits 1)
	      ((query-opcode->value (dns-message-opcode m)) : bits 4)
	      ((boolean->bit (dns-message-authoritative? m)) : bits 1)
	      ((boolean->bit (dns-message-truncated? m)) : bits 1)
	      ((boolean->bit (dns-message-recursion-desired? m)) : bits 1)
	      ((boolean->bit (dns-message-recursion-available? m)) : bits 1)
	      (0 : bits 3)
	      ((query-response-code->value (dns-message-response-code m)) : bits 4)
	      ((length (dns-message-questions m)) : bits 16)
	      ((length (dns-message-answers m)) : bits 16)
	      ((length (dns-message-authorities m)) : bits 16)
	      ((length (dns-message-additional m)) : bits 16)
	      ((bit-string-append
		(encode-section encode-question (dns-message-questions m))
		(encode-section encode-rr (dns-message-answers m))
		(encode-section encode-rr (dns-message-authorities m))
		(encode-section encode-rr (dns-message-additional m))) : binary)))

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

    ([(= 3 : bits 2) (offset : bits 14) (rest : binary)]
     (if (member offset pointers-followed)
	 (error 'parse-domain-name "DNS compressed-pointer loop detected")
	 (let-values (((lhs rhs) (bit-string-split-at whole-packet (* 8 offset))))
	   (let-values (((labels ignored-tail)
			 (parse-domain-name whole-packet rhs (cons offset pointers-followed))))
	     (values labels rest)))))

    ([(= 0 : bits 8) (rest : binary)]
     (values '() rest))

    ([(= 0 : bits 2) (len : bits 6) (label : binary bytes len) (rest : binary)]
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

;; Character strings are pascal-style length-byte-prefixed strings.

(define (extract-character-strings input)
  (bit-string-case input
    ([]
     '())
    ([len (body : binary bytes len) (rest : binary)]
     (cons (bit-string->bytes body)
	   (extract-character-strings rest)))))

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
      ([(qtype : bits 16)
	(qclass : bits 16)
	(tail : binary)]
       (values (question qname
			 (value->qtype qtype)
			 (value->qclass qclass))
	       tail)))))

(define (encode-question q)
  (error 'bang))

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
      ([(type-number : bits 16)
	(class : bits 16)
	(ttl : bits 32)
	(rdlength : bits 16)
	(rdata : binary bytes rdlength)
	(tail : binary)]
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
	    ([(preference : bits 16) (exchange : binary)]
	     (mx preference (parse-single-domain-name whole-packet exchange)))))
    ((null) (bit-string->bytes rdata))
    ((soa) (let*-values (((mname rdata1) (parse-domain-name whole-packet rdata))
			 ((rname rdata2) (parse-domain-name whole-packet rdata1)))
	     (bit-string-case rdata2
	       ([(serial : bits 32)
		 (refresh : bits 32)
		 (retry : bits 32)
		 (expire : bits 32)
		 (minimum : bits 32)]
		(soa mname rname serial refresh retry expire minimum)))))
    ((txt) (extract-character-strings rdata))
    ((a) (bit-string-case rdata
	   ([a b c d]
	    (vector a b c d))))
    ((wks) (bit-string-case rdata
	     ([a b c d protocol (bitmap : binary)]
	      (wks (vector a b c d) protocol bitmap))))
    (else (bit-string->bytes rdata))))

(define (encode-rr rr)
  (error 'bang))

;;---------------------------------------------------------------------------

;;(define ns-addr "192.168.1.1")
;;(define ns-port 53)
(define ns-addr "127.0.0.1")
(define ns-port 9999)

(define local-addr "0.0.0.0")

(define s (udp-open-socket local-addr #f))
(udp-bind! s local-addr 0)

(define local-port
  (let-values (((local-addr local-port remote-addr remote-port)
		(udp-addresses s #t)))
    local-port))
(printf "Local port number is ~v~n" local-port)

;;(udp-send-to s ns-addr ns-port #"hello")

;; Wed Jun 29 16:33:58 2011 (4e0b8c36): UDP: localhost sent 28 bytes:
;; 00000000: 66 3A 01 00 00 01 00 00 : 00 00 00 00 06 67 6F 6F  f:...........goo
;; 00000010: 67 6C 65 03 63 6F 6D 00 : 00 FF 00 01              gle.com.....
;; 0000001C:

(define (q-google-in-any)
  (bytes #x66 #x3A #x01 #x00 #x00 #x01 #x00 #x00
	 #x00 #x00 #x00 #x00 #x06 #x67 #x6F #x6F
	 #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00
	 #x00 #xFF #x00 #x01))

;; Wed Jun 29 16:33:58 2011 (4e0b8c36): UDP: dslrouter.westell.com sent 494 bytes:
;; 00000000: 66 3A 81 80 00 01 00 0F : 00 00 00 07 06 67 6F 6F  f:...........goo
;; 00000010: 67 6C 65 03 63 6F 6D 00 : 00 FF 00 01 C0 0C 00 10  gle.com.........
;; 00000020: 00 01 00 00 0C 2F 00 52 : 51 76 3D 73 70 66 31 20  ...../.RQv=spf1 
;; 00000030: 69 6E 63 6C 75 64 65 3A : 5F 6E 65 74 62 6C 6F 63  include:_netbloc
;; 00000040: 6B 73 2E 67 6F 6F 67 6C : 65 2E 63 6F 6D 20 69 70  ks.google.com ip
;; 00000050: 34 3A 32 31 36 2E 37 33 : 2E 39 33 2E 37 30 2F 33  4:216.73.93.70/3
;; 00000060: 31 20 69 70 34 3A 32 31 : 36 2E 37 33 2E 39 33 2E  1 ip4:216.73.93.
;; 00000070: 37 32 2F 33 31 20 7E 61 : 6C 6C C0 0C 00 01 00 01  72/31 ~all......
;; 00000080: 00 00 01 1D 00 04 4A 7D : E2 92 C0 0C 00 01 00 01  ......J}........
;; 00000090: 00 00 01 1D 00 04 4A 7D : E2 94 C0 0C 00 01 00 01  ......J}........
;; 000000A0: 00 00 01 1D 00 04 4A 7D : E2 91 C0 0C 00 01 00 01  ......J}........
;; 000000B0: 00 00 01 1D 00 04 4A 7D : E2 93 C0 0C 00 01 00 01  ......J}........
;; 000000C0: 00 00 01 1D 00 04 4A 7D : E2 90 C0 0C 00 02 00 01  ......J}........
;; 000000D0: 00 03 A5 1D 00 06 03 6E : 73 32 C0 0C C0 0C 00 02  .......ns2......
;; 000000E0: 00 01 00 03 A5 1D 00 06 : 03 6E 73 33 C0 0C C0 0C  .........ns3....
;; 000000F0: 00 02 00 01 00 03 A5 1D : 00 06 03 6E 73 31 C0 0C  ...........ns1..
;; 00000100: C0 0C 00 02 00 01 00 03 : A5 1D 00 06 03 6E 73 34  .............ns4
;; 00000110: C0 0C C0 0C 00 0F 00 01 : 00 00 00 2A 00 11 00 14  ...........*....
;; 00000120: 04 61 6C 74 31 05 61 73 : 70 6D 78 01 6C C0 0C C0  .alt1.aspmx.l...
;; 00000130: 0C 00 0F 00 01 00 00 00 : 2A 00 09 00 1E 04 61 6C  ........*.....al
;; 00000140: 74 32 C1 25 C0 0C 00 0F : 00 01 00 00 00 2A 00 04  t2.%.........*..
;; 00000150: 00 0A C1 25 C0 0C 00 0F : 00 01 00 00 00 2A 00 09  ...%.........*..
;; 00000160: 00 28 04 61 6C 74 33 C1 : 25 C0 0C 00 0F 00 01 00  .(.alt3.%.......
;; 00000170: 00 00 2A 00 09 00 32 04 : 61 6C 74 34 C1 25 C0 E8  ..*...2.alt4.%..
;; 00000180: 00 01 00 01 00 03 A2 CF : 00 04 D8 EF 24 0A C0 FA  ............$...
;; 00000190: 00 01 00 01 00 03 A2 CF : 00 04 D8 EF 20 0A C1 0C  ............ ...
;; 000001A0: 00 01 00 01 00 03 A2 CF : 00 04 D8 EF 26 0A C0 D6  ............&...
;; 000001B0: 00 01 00 01 00 03 A2 CF : 00 04 D8 EF 22 0A C1 3D  ............"..=
;; 000001C0: 00 01 00 01 00 00 00 F0 : 00 04 4A 7D 27 1B C1 25  ..........J}'..%
;; 000001D0: 00 01 00 01 00 00 00 F6 : 00 04 4A 7D 73 1B C1 20  ..........J}s.. 
;; 000001E0: 00 01 00 01 00 00 00 21 : 00 04 4A 7D 4D 1B        .......!..J}M.
;; 000001EE:

(define (a-google-in-any)
  (bytes
   #x66 #x3A #x81 #x80 #x00 #x01 #x00 #x0F #x00 #x00 #x00 #x07 #x06 #x67 #x6F #x6F
   #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00 #x00 #xFF #x00 #x01 #xC0 #x0C #x00 #x10
   #x00 #x01 #x00 #x00 #x0C #x2F #x00 #x52 #x51 #x76 #x3D #x73 #x70 #x66 #x31 #x20
   #x69 #x6E #x63 #x6C #x75 #x64 #x65 #x3A #x5F #x6E #x65 #x74 #x62 #x6C #x6F #x63
   #x6B #x73 #x2E #x67 #x6F #x6F #x67 #x6C #x65 #x2E #x63 #x6F #x6D #x20 #x69 #x70
   #x34 #x3A #x32 #x31 #x36 #x2E #x37 #x33 #x2E #x39 #x33 #x2E #x37 #x30 #x2F #x33
   #x31 #x20 #x69 #x70 #x34 #x3A #x32 #x31 #x36 #x2E #x37 #x33 #x2E #x39 #x33 #x2E
   #x37 #x32 #x2F #x33 #x31 #x20 #x7E #x61 #x6C #x6C #xC0 #x0C #x00 #x01 #x00 #x01
   #x00 #x00 #x01 #x1D #x00 #x04 #x4A #x7D #xE2 #x92 #xC0 #x0C #x00 #x01 #x00 #x01
   #x00 #x00 #x01 #x1D #x00 #x04 #x4A #x7D #xE2 #x94 #xC0 #x0C #x00 #x01 #x00 #x01
   #x00 #x00 #x01 #x1D #x00 #x04 #x4A #x7D #xE2 #x91 #xC0 #x0C #x00 #x01 #x00 #x01
   #x00 #x00 #x01 #x1D #x00 #x04 #x4A #x7D #xE2 #x93 #xC0 #x0C #x00 #x01 #x00 #x01
   #x00 #x00 #x01 #x1D #x00 #x04 #x4A #x7D #xE2 #x90 #xC0 #x0C #x00 #x02 #x00 #x01
   #x00 #x03 #xA5 #x1D #x00 #x06 #x03 #x6E #x73 #x32 #xC0 #x0C #xC0 #x0C #x00 #x02
   #x00 #x01 #x00 #x03 #xA5 #x1D #x00 #x06 #x03 #x6E #x73 #x33 #xC0 #x0C #xC0 #x0C
   #x00 #x02 #x00 #x01 #x00 #x03 #xA5 #x1D #x00 #x06 #x03 #x6E #x73 #x31 #xC0 #x0C
   #xC0 #x0C #x00 #x02 #x00 #x01 #x00 #x03 #xA5 #x1D #x00 #x06 #x03 #x6E #x73 #x34
   #xC0 #x0C #xC0 #x0C #x00 #x0F #x00 #x01 #x00 #x00 #x00 #x2A #x00 #x11 #x00 #x14
   #x04 #x61 #x6C #x74 #x31 #x05 #x61 #x73 #x70 #x6D #x78 #x01 #x6C #xC0 #x0C #xC0
   #x0C #x00 #x0F #x00 #x01 #x00 #x00 #x00 #x2A #x00 #x09 #x00 #x1E #x04 #x61 #x6C
   #x74 #x32 #xC1 #x25 #xC0 #x0C #x00 #x0F #x00 #x01 #x00 #x00 #x00 #x2A #x00 #x04
   #x00 #x0A #xC1 #x25 #xC0 #x0C #x00 #x0F #x00 #x01 #x00 #x00 #x00 #x2A #x00 #x09
   #x00 #x28 #x04 #x61 #x6C #x74 #x33 #xC1 #x25 #xC0 #x0C #x00 #x0F #x00 #x01 #x00
   #x00 #x00 #x2A #x00 #x09 #x00 #x32 #x04 #x61 #x6C #x74 #x34 #xC1 #x25 #xC0 #xE8
   #x00 #x01 #x00 #x01 #x00 #x03 #xA2 #xCF #x00 #x04 #xD8 #xEF #x24 #x0A #xC0 #xFA
   #x00 #x01 #x00 #x01 #x00 #x03 #xA2 #xCF #x00 #x04 #xD8 #xEF #x20 #x0A #xC1 #x0C
   #x00 #x01 #x00 #x01 #x00 #x03 #xA2 #xCF #x00 #x04 #xD8 #xEF #x26 #x0A #xC0 #xD6
   #x00 #x01 #x00 #x01 #x00 #x03 #xA2 #xCF #x00 #x04 #xD8 #xEF #x22 #x0A #xC1 #x3D
   #x00 #x01 #x00 #x01 #x00 #x00 #x00 #xF0 #x00 #x04 #x4A #x7D #x27 #x1B #xC1 #x25
   #x00 #x01 #x00 #x01 #x00 #x00 #x00 #xF6 #x00 #x04 #x4A #x7D #x73 #x1B #xC1 #x20
   #x00 #x01 #x00 #x01 #x00 #x00 #x00 #x21 #x00 #x04 #x4A #x7D #x4D #x1B))

(require racket/pretty)
(pretty-print (packet->dns-message (q-google-in-any)))
(pretty-print (packet->dns-message (a-google-in-any)))

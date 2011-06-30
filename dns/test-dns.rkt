#lang racket/base

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

(pretty-print (dns-message->packet (packet->dns-message (a-google-in-any))))

;; Wed Jun 29 20:59:17 2011 (4e0bca65): UDP: localhost sent 28 bytes:
;; 00000000: 47 16 01 00 00 01 00 00 : 00 00 00 00 06 67 6F 6F  G............goo
;; 00000010: 67 6C 65 03 63 6F 6D 00 : 00 1C 00 01              gle.com.....
;; 0000001C:
;; Wed Jun 29 20:59:17 2011 (4e0bca65): UDP: pass through succeeded
;; Wed Jun 29 20:59:17 2011 (4e0bca65): UDP: google-public-dns-a.google.com sent 78 bytes:
;; 00000000: 47 16 81 80 00 01 00 00 : 00 01 00 00 06 67 6F 6F  G............goo
;; 00000010: 67 6C 65 03 63 6F 6D 00 : 00 1C 00 01 C0 0C 00 06  gle.com.........
;; 00000020: 00 01 00 00 02 52 00 26 : 03 6E 73 31 C0 0C 09 64  .....R.&.ns1...d
;; 00000030: 6E 73 2D 61 64 6D 69 6E : C0 0C 00 16 33 23 00 00  ns-admin....3#..
;; 00000040: 1C 20 00 00 07 08 00 12 : 75 00 00 00 01 2C        . ......u....,
;; 0000004E:

(pretty-print
 (packet->dns-message
  (bytes
   #x47 #x16 #x01 #x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x06 #x67 #x6F #x6F
   #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00 #x00 #x1C #x00 #x01)))

(pretty-print
 (packet->dns-message
  (bytes
   #x47 #x16 #x81 #x80 #x00 #x01 #x00 #x00 #x00 #x01 #x00 #x00 #x06 #x67 #x6F #x6F
   #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00 #x00 #x1C #x00 #x01 #xC0 #x0C #x00 #x06
   #x00 #x01 #x00 #x00 #x02 #x52 #x00 #x26 #x03 #x6E #x73 #x31 #xC0 #x0C #x09 #x64
   #x6E #x73 #x2D #x61 #x64 #x6D #x69 #x6E #xC0 #x0C #x00 #x16 #x33 #x23 #x00 #x00
   #x1C #x20 #x00 #x00 #x07 #x08 #x00 #x12 #x75 #x00 #x00 #x00 #x01 #x2C)))

;; Wed Jun 29 21:05:03 2011 (4e0bcbbf): UDP: localhost sent 32 bytes:
;; 00000000: 12 70 01 00 00 01 00 00 : 00 00 00 00 03 77 77 77  .p...........www
;; 00000010: 06 67 6F 6F 67 6C 65 03 : 63 6F 6D 00 00 1C 00 01  .google.com.....
;; 00000020:
;; Wed Jun 29 21:05:03 2011 (4e0bcbbf): UDP: pass through succeeded
;; Wed Jun 29 21:05:03 2011 (4e0bcbbf): UDP: ns1.google.com sent 52 bytes:
;; 00000000: 12 70 85 00 00 01 00 01 : 00 00 00 00 03 77 77 77  .p...........www
;; 00000010: 06 67 6F 6F 67 6C 65 03 : 63 6F 6D 00 00 1C 00 01  .google.com.....
;; 00000020: C0 0C 00 05 00 01 00 09 : 3A 80 00 08 03 77 77 77  ........:....www
;; 00000030: 01 6C C0 10             :                          .l..
;; 00000034:

(pretty-print
 (packet->dns-message
  (bytes
   #x12 #x70 #x01 #x00 #x00 #x01 #x00 #x00 #x00 #x00 #x00 #x00 #x03 #x77 #x77 #x77
   #x06 #x67 #x6F #x6F #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00 #x00 #x1C #x00 #x01)))

(pretty-print
 (packet->dns-message
  (bytes
   #x12 #x70 #x85 #x00 #x00 #x01 #x00 #x01 #x00 #x00 #x00 #x00 #x03 #x77 #x77 #x77
   #x06 #x67 #x6F #x6F #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00 #x00 #x1C #x00 #x01
   #xC0 #x0C #x00 #x05 #x00 #x01 #x00 #x09 #x3A #x80 #x00 #x08 #x03 #x77 #x77 #x77
   #x01 #x6C #xC0 #x10)))

;; Wed Jun 29 21:07:46 2011 (4e0bcc62): UDP: ns1.google.com sent 82 bytes:
;; 00000000: 23 79 85 00 00 01 00 02 : 00 00 00 00 04 69 70 76  #y...........ipv
;; 00000010: 36 06 67 6F 6F 67 6C 65 : 03 63 6F 6D 00 00 1C 00  6.google.com....
;; 00000020: 01 C0 0C 00 05 00 01 00 : 09 3A 80 00 09 04 69 70  .........:....ip
;; 00000030: 76 36 01 6C C0 11 C0 2D : 00 1C 00 01 00 00 01 2C  v6.l...-.......,
;; 00000040: 00 10 20 01 48 60 80 0F : 00 00 00 00 00 00 00 00  .. .H`..........
;; 00000050: 00 68                   :                          .h
;; 00000052:

(pretty-print
 (packet->dns-message
  (bytes
   #x23 #x79 #x85 #x00 #x00 #x01 #x00 #x02 #x00 #x00 #x00 #x00 #x04 #x69 #x70 #x76
   #x36 #x06 #x67 #x6F #x6F #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00 #x00 #x1C #x00
   #x01 #xC0 #x0C #x00 #x05 #x00 #x01 #x00 #x09 #x3A #x80 #x00 #x09 #x04 #x69 #x70
   #x76 #x36 #x01 #x6C #xC0 #x11 #xC0 #x2D #x00 #x1C #x00 #x01 #x00 #x00 #x01 #x2C
   #x00 #x10 #x20 #x01 #x48 #x60 #x80 #x0F #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
   #x00 #x68)))

(pretty-print
 (dns-message->packet
  (packet->dns-message
   (bytes
    #x23 #x79 #x85 #x00 #x00 #x01 #x00 #x02 #x00 #x00 #x00 #x00 #x04 #x69 #x70 #x76
    #x36 #x06 #x67 #x6F #x6F #x67 #x6C #x65 #x03 #x63 #x6F #x6D #x00 #x00 #x1C #x00
    #x01 #xC0 #x0C #x00 #x05 #x00 #x01 #x00 #x09 #x3A #x80 #x00 #x09 #x04 #x69 #x70
    #x76 #x36 #x01 #x6C #xC0 #x11 #xC0 #x2D #x00 #x1C #x00 #x01 #x00 #x00 #x01 #x2C
    #x00 #x10 #x20 #x01 #x48 #x60 #x80 #x0F #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
    #x00 #x68))))

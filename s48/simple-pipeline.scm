(define (simple-pipeline input-string command . args)
  (let* ((results (apply process* command args))
	 (stdout (car results))
	 (stdin (cadr results))
	 (stderr (cadddr results))
	 (controller (car (cddddr results))))
    (display input-string stdin)
    (close-output-port stdin)
    (controller 'wait)
    (values (port->string/close stdout)
	    (port->string/close stderr))))

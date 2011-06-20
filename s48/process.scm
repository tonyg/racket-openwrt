;; Scheme48 process*, following the Racket API, in the style of
;; misc/pipe.scm from the Scheme48 source distribution.

(define (controller pid)
  (let ((cached-status #f)
	(cached-code #f))
    (define (determine)
      (if (not cached-status)
	  (cond
	   ((process-id-exit-status pid) =>
	    (lambda (code)
	      (set! cached-status (if (zero? code)
				      'done-ok
				      'done-error))
	      (set! cached-code code)))
	   ((process-id-terminating-signal pid) =>
	    (lambda (sig)
	      (set! cached-status 'done-signal)
	      (set! cached-code sig)))
	   (else 'nothing-yet)))
      cached-status)
    (lambda (operation)
      (case operation
	((status) (or (determine) 'running))
	((exit-code) (case (determine)
		       ((done-ok done-error) cached-code)
		       (else #f)))
	((exit-signal) (case (determine)
			 ((done-signal) cached-code)
			 (else #f)))
	((wait) (wait-for-child-process pid))
	((interrupt) (signal-process pid 'int))
	((kill) (signal-process pid 'kill))
	(else (error "Invalid process controller operation"
		     operation))))))

(define (process command)
  (process* "/bin/sh" "-c" command))

(define (process* command . args)
  (call-with-values open-pipe
    (lambda (parent-from-child child-to-parent)
      (call-with-values open-pipe
	(lambda (child-from-parent parent-to-child)
	  (call-with-values open-pipe
	    (lambda (stderr-from-child child-to-stderr)
	      (let ((pid (fork)))
		(if pid
		    ;; Parent
		    (begin (close-input-port child-from-parent)
			   (close-output-port child-to-parent)
			   (close-output-port child-to-stderr)
			   (list parent-from-child
				 parent-to-child
				 (process-id->integer pid) ;; boo
				 stderr-from-child
				 (controller pid)))
		    ;; Child
		    (begin (close-all-but child-from-parent
					  child-to-parent
					  child-to-stderr)
			   (with-exception-handler
			    (lambda (exn) (exit 1))
			    (lambda ()
			      (remap-file-descriptors! child-from-parent
						       child-to-parent
						       child-to-stderr)
			      (apply exec command args)))))))))))))


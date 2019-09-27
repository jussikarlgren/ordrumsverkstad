; socket.scm - proprietary to gavagai ab
; www.gavagai.se
;--------------------------------------------------------
; * SOCKET MODULE
; functions for socket access to gavagai core
;--------------------------------------------------------
; * REVISION HISTORY
; 2010-05-10 JK: added LEARN variable
; 2010-04-19 JK: added select struct
; 2010-04-07 JK: diversified from original core.scm
;--------------------------------------------------------


(sigaction SIGPIPE (lambda (i) 
		     (let ((scratch (open-file (string-append "error-" LABEL "-" global-id-string) "a")))
		       (begin
			 (display (date->string (time-utc->date (current-time)) "~5") scratch)
			 (display "Received the SIGPIPE signal!!!" scratch)
			 (newline scratch)
			 (close scratch)
			 ))))



(define (preprocess txt) 
  (if (eof-object? txt)
      "null"
      (regexp-substitute/global #f "[0-9a-z]{50,}" 
				(regexp-substitute/global #f "[^0-9a-z&=]" (string-downcase! txt) 'pre " " 'post) 
				'pre " " 'post)))


(define input-regex "(id=([a-z0-9]+)&txt=([a-z0-9 ]+))")

(define (get-id match-structure) 
  (if (> (match:count match-structure) 1) 
      (match:substring match-structure 2) "no-id"))
(define (get-txt match-structure) 
  (if (> (match:count match-structure) 2) 
      (match:substring match-structure 3)
      ""))



; add new text to the word space from socket - but via ugly TMPFILE hack solution! 
; this NEEDSNEEDSNEEDS to be FIXED!!
(define (add-new-text-via-tmpfile id file)
  (check-text-sections "***")
  (if CLEAR
      (begin
	(if the_text
	        (close-text the_text))
	(set! nr_words 0)
	(set! nr_docs 0)
	(set! the_text (open-text file))
	(register-all-words))
      (begin
	(if the_text
	        (append-text the_text file)
		    (set! the_text (open-text file)))
(display id) (display " ") (display nr_words)(display " ")(display nr_docs)
(newline)
(register-new-words)
))
  )



; a number of global variables, useful in case of crash
(define global-id-string (number->string (getpid)))
(define global-received-text-counter 0)

(define (gavagai-socket)
  (let* (
	 (headsocket (socket PF_INET SOCK_STREAM 0))
	)
    (if BGDATA (import-words BGDATA))
    (setsockopt headsocket SOL_SOCKET SO_REUSEADDR 1)
    (bind headsocket AF_INET INADDR_ANY PORT)
    (listen headsocket 20)
    (display "Listening for clients in: ")
    (display headsocket)  
    (newline)
    (let ((socketlist (list headsocket)))
	     (catch 
	      #t
	      (socket-loop headsocket socketlist)
	      (lambda (key . args)					; catch exception 
		(let ((scratch (open-file (string-append global-id-string "-" LABEL ".log") "a")))
		  (display key scratch)
		  (display "->" scratch)
		  (display args scratch)
		  (display "<- at text number: " scratch)
		  (display global-received-text-counter scratch)
		  (display " " scratch)
		  (display (date->string (time-utc->date (current-time)) "~5") scratch)
		  (newline scratch)      
		  (newline scratch)
		  (close scratch)
		  (if (eq? key 'system-error) (map close socketlist))
		  (if (eq? key 'misc-error) (socket-loop headsocket socketlist))
		  ))
	      ); catch
	     ); let socketlist
    ); let*
  ); define
;==================================================
(define (socket-loop headsocket socketlist)
  (let 	  ((flag #t))
    (while flag
		       (let ((activesockets (select socketlist '() '() 3)))
			 (if activesockets
			     (map (lambda (onesocket)
				    (cond 
				     ((eq? onesocket headsocket)
				      (begin
					(set! socketlist (cons (car (accept onesocket)) socketlist))
					      (display "caught new socket: ") ; debug
					      (display onesocket)        ; debug
					      (newline)                  ; debug
					)
				      ) ; cond-onesocket=headsocket
;==================================================
				     (onesocket
				      (let (
					    (id "4711") ; dummy
					    (hep "hep") ; dummy
					    (line (read-line onesocket))
					    )
					(if (or (eof-object? line) (string=? line "bye") (string=? line "\n"))
					    (begin
;					      (display "end of input: ") ; debug
;					      (display onesocket)        ; debug
;					      (newline)                  ; debug
					      (shutdown onesocket 0)
					      (set! socketlist (delq! onesocket socketlist)))
					    (let  
						((mtc (string-match input-regex (preprocess line))) 
						 )
					      (if mtc (begin
							(set! id (get-id mtc))
							(set! hep (get-txt mtc))
							(if LEARN 
							    (let 
								((scratch (open-file (string-append UGLY_HACK_TMPFILE "-" global-id-string) "w"))
								 )
							      (display id scratch)
							      (display " " scratch)
							      (display hep scratch)
							      (newline scratch) (newline scratch)
							      (close scratch)
							      (add-new-text-via-tmpfile id (string-append UGLY_HACK_TMPFILE "-" global-id-string))
							      )
							    (if (defined? (quote OUTFILE5))
								(if (and OUTFILE5 (> global-received-text-counter 10000) )
								    (let (
									  (scotch (open-file (string-append global-id-string "-" LABEL ".log") "a")))
								      (begin-thread (export-words OUTFILE5))
								      (set! global-received-text-counter 0)
								      (display (string-append "exporting words to " OUTFILE5) scotch)
								      (newline scotch)
								      (close scotch)
								      );let
								    );if outfile 10000
								);if outifle5
							    ) ; if LEARN   
							(display (string-append "id=" id) onesocket)
							(display (socket-process hep) onesocket)
							(display "\r\n" onesocket)
							(if LEARN (train))
							(+ global-received-text-counter 1)
							)); if mtc
					      ); let
					    ); if eof
					)) ; cond-onesocket
;==================================================
				     (#t  
					(set! flag #f)
				      ); cond else
				     ); cond
				    ); lambda 
				  (car activesockets)); map
			     (set! flag #f)
) ; if activesockets
			 ); let activesockets
		       ); while flag
);let flag
); define
;==================================================
(define (socket-process utterance)
  (let (
	(res "") 
	(polehash (setup-poleword-hash POLES))
	(words (string-split utterance #\sp))
	)
    (hash-for-each (lambda (k v)
		     (let* ((ny (make-compositional-new words v)))
		       (set! res (string-append res "&" k "=" (number->string ny)))))
		   polehash)
    res ; return result
    ); let
  ); define


; anal.scm - proprietary to gavagai ab
; www.gavagai.se
;--------------------------------------------------------
; * ANALYSIS MODULE
; functions for polarity and associative analyses
;--------------------------------------------------------
; * REVISION HISTORY
; 2010-05-05 MS+JK: new compositional utterance-by-utterance analysis
; 2010-04-10 JK: moved make-compositional-string here from 
;                socket development branch. as make-compositional,
;                but returns string instead of writing to file. 
; 2009-11-16 MS: added (associate-stem...)
;                and removed freq output for nns
; 2009-10-13 MS: compositional function added
; 2009-09-16 MS: functions moved to module
;                associative analysis added
;--------------------------------------------------------
; computes similarities between targets and poles
; and appends the result at the end of the outfile
; the format of the outfile is:
; YEAR-MONTH-DAY:HOUR target pole sim pole sim pole sim ...\n
(define (polarize polehash targethash time)
  (let ((out (open-file OUTFILE1 "a")))
    (hash-for-each (lambda (t_k tv)
		     (display time out)(display " " out)
		     (display t_k out)
		     (if tv
			 (hash-for-each
			  (lambda (pk pv)
			    (display " " out) (display pk out) (display " " out)
			    (if pv
				(display (sparse-corr tv pv) out)
				(display "#f" out)))
			  polehash)
			 (begin (display " #f" out)))
		     (newline out))
		   targethash)
    (close-port out)))

; reads utternace and acucmulates cosines against polewords
(define (make-compositional-new utterance polewords)
  (let ((accum 0)
	(flipflag 1)
	(hedgefactor 1))
    (for-each (lambda (w)
(let ((word (lookup-word w)))
		(if (and (negation? w) FLIP)
		    (set! flipflag -1))
		(if (and (hedge? w) HEDGE)
		    (set! hedgefactor HEDGE))
		(if (good-word? word dir_ctx NULLWORD)
		    (for-each (lambda (h)
				(let ((pword (lookup-word h)))
				(if (and pword (get-dir_ctx pword) (get-dir_ctx word))
				    (let ((nn (sparse-corr (get-dir_ctx word)(get-dir_ctx pword))))
				      (if (and (number? nn) (not (nan? nn)) (not (inf? nn)))
					  (set! accum (+ accum (expt nn EXPVAR)))
					  )
				      )
				    )
				))
			      polewords)
		    )))
	      utterance)
    (* accum flipflag hedgefactor)
))

; extracts the N nearest neighbors to pole vectors in targethash
; the format of the outfile is:
; YEAR-MONTH-DAY:HOUR target nn1 sim nn2 sim ...\n
(define (associate-poles analhash vec time outfile)
  (let ((out (open-file outfile "a")))
    (hash-for-each
     (lambda (tk tv)
       (display time out)(display " " out)
       (display tk out) (display " " out)
       (if tv
	   (let ((res '()))
	     (for-each-unique-word
	      (lambda (wrd)
		(if (and (not (targetword? wrd))
			 (list-ref wrd vec)
			 (not (member (car wrd) STOP))
			 (>= (get-freq wrd) MINFREQ)
			 (not (string-prefix? "###" (car wrd))))
		    (set! res (cons (list (car wrd)
					  (sparse-corr tv (list-ref wrd vec)))
				    res)))))
	     (for-each (lambda (nn)
			 (display (car nn) out)(display " " out)
			 (display (cadr nn) out)(display " " out))
		       (if (< NR_ASS (length res))
			   (list-head (sort res (lambda (x1 x2)
						  (> (cadr x1) (cadr x2))
						  )) NR_ASS)
			   (sort res (lambda (x1 x2)
				       (> (cadr x1) (cadr x2))))))
	     (newline out))
	   (display "#f\n" out)))
     analhash)))

; extracts the N nearest neighbors and outputs the result
; the format of the outfile is:
; YEAR-MONTH-DAY:HOUR target freq nn1 sim nn2 sim ...\n
(define (associate wrdlist vec time outfile)
  (let ((out (open-file outfile "a"))
	(stem #f))
    (for-each
     (lambda (wrd)
       (if (string-prefix? "*" wrd)
	   (if (string-suffix? "*" wrd)
	       (set! stem (string-drop-right (string-drop wrd 1) 1))
	       (set! stem (string-drop wrd 1)))
	   (if (string-suffix? "*" wrd)
	       (set! stem (string-drop-right wrd 1))
	       (set! stem wrd)))
       (display time out)(display " " out)
       (display stem out)(display " " out)
       (let ((w (lookup-word stem))
	     (res '()))
	 (if (and w (list-ref w vec))
	     (begin
	       (for-each-unique-word
		(lambda (wrd2)
		  (if (and (not (eq? wrd2 w))
			   (list-ref wrd2 vec)
			   (not (member (car wrd2) STOP))
			   (>= (get-freq wrd2) MINFREQ)
			   (not (string-prefix? "###" (car wrd2))))
		      (set! res (cons 
				 (list (car wrd2)
				       (sparse-corr (list-ref wrd2 vec)
						    (list-ref w vec)))
;				       (get-freq wrd2))
				 res)))))
	       (display (get-freq w) out)(display " " out)
	       (for-each (lambda (nn)
			   (display (car nn) out)(display " " out)
			   (display (cadr nn) out)(display " " out))
;			   (display (caddr nn) out)(display " " out))
			 (list-head (sort res (lambda (x1 x2)
						(> (cadr x1) (cadr x2)))) NR_ASS))
	       (newline out))
	     (display "#f\n" out))))
     wrdlist)))

; help function to (associate-stem...)
(define (filler wrd nnvec reshash)
  (for-each-unique-word
   (lambda (target)
     (if (and (not (eq? (cdr target) (cdr wrd)))
	      (list-ref target nnvec)
	      (not (member (car target) STOP))
	      (>= (list-ref target freq) MINFREQ)
	      (not (string-prefix? "###" (car target)))
	      (not (string-prefix? "***" (car target))))
	 (if (hash-ref reshash (car target))
	     (hash-set! reshash (car target)
			(cons
			 (+ (car (hash-ref reshash (car target)))
			    (sparse-corr (list-ref target nnvec)
					 (list-ref wrd nnvec)))
			 (1+ (cdr (hash-ref reshash (car target))))))
	     (hash-set! reshash (car target)
			(cons
			 (sparse-corr (list-ref target nnvec)
				      (list-ref wrd nnvec))
			 1)))))))

; extract the num nearest neighbors to stem
; by extracting nns for all words with stem
; output number of stem words instead of freq
(define (associate-stem nnvec time outfile)
  (let* ((out (open-file outfile "a"))
	 (in (open-input-file TARGETS))
	 (line (read-line in)))
    (while (not (eof-object? line))
	   (let* ((splitted (string-split line #\sp))
		  (label (car splitted))
		  (wordlist (cdr splitted))
		  (res (make-hash-table))
		  (ret '())(flag #f)(cnt 0))
	     (for-each
	      (lambda (wrd)
		(if (string-prefix? "*" wrd)
		    (let ((stem (string-drop wrd 1)))
		      (if (string-suffix? "*" stem)
			  (let ((infix (string-drop-right stem 1)))
			    (for-each-unique-word
			     (lambda (st)
			       (if (and (string-contains (car st) infix)
					(list-ref st nnvec))
				   (begin
				     (set! flag #t)(set! cnt (1+ cnt))
				     (filler st nnvec res)))))
			    (for-each-unique-word
			     (lambda (st)
			       (if (and (string-suffix? stem (car st))
					(list-ref st nnvec))
				   (begin
				     (set! flag #t)(set! cnt (1+ cnt))
				     (filler st nnvec res))))))))
		    (if (string-suffix? "*" wrd)
			(let ((prefix (string-drop-right wrd 1)))
			  (for-each-unique-word
			   (lambda (st)
			     (if (and (string-prefix? prefix (car st))
				      (list-ref st nnvec))
				 (begin
				   (set! flag #t)(set! cnt (1+ cnt))
				   (filler st nnvec res))))))
			(let ((w (lookup-word wrd)))
			  (if (and w (list-ref w nnvec))
			      (begin
				(set! flag #t)(set! cnt (1+ cnt))
				(filler w nnvec res)))))))
	      wordlist)
	     (display time out)(display " " out)
	     (display label out)(display " " out)
	     (display cnt out)(display " " out)
	     (if flag
		 (begin
		   (hash-for-each (lambda (k v)
				    (set! ret (cons
					       (list k (/ (car v) (cdr v))) ret)))
				  res)
		   (for-each (lambda (r) (display (car r) out) (display " " out)
				     (let ((nr (number->string (cadr r))))
				       (if (> (string-length nr) 5)
					   (display (substring nr 0 5) out)
					   (display nr out))
				       (display " " out)))
			     (list-head (sort ret (lambda (x1 x2)
						    (> (cadr x1) (cadr x2))))
					NR_ASS))
		   (newline out))
		 (display "#f\n" out)))
	   (set! line (read-line in)))))

(define (associate-stem-centroid nnvec time outfile)
  (let* ((out (open-file outfile "a"))
	 (in (open-input-file TARGETS))
	 (line (read-line in)))
    (while (not (eof-object? line))
	   (let* ((splitted (string-split line #\sp))
		  (label (car splitted))
		  (wordlist (cdr splitted))
		  (vec (make-dense VEC))
		  (ret '())(words '())(flag #f)(cnt 0))
	     (for-each
	      (lambda (wrd)
		(if (string-prefix? "*" wrd)
		    (let ((stem (string-drop wrd 1)))
		      (if (string-suffix? "*" stem)
			  (let ((infix (string-drop-right stem 1)))
			    (for-each-unique-word
			     (lambda (st)
			       (if (and (string-contains (car st) infix)
					(list-ref st nnvec))
				   (begin 
				     (set! flag #t)(set! cnt (1+ cnt))
				     (set! words (append words (list (car st))))
				     (sparse-add! vec (list-ref st nnvec)))))))
			  (for-each-unique-word
			   (lambda (st)
			     (if (and (string-suffix? stem (car st))
				      (list-ref st nnvec))
				 (begin 
				   (set! flag #t)(set! cnt (1+ cnt))
				   (set! words (append words (list (car st))))
				   (sparse-add! vec (list-ref st nnvec))))))))
		    (if (string-suffix? "*" wrd)
			(let ((prefix (string-drop-right wrd 1)))
			  (for-each-unique-word
			   (lambda (st)
			     (if (and (string-prefix? prefix (car st))
				      (list-ref st nnvec))
				 (begin 
				   (set! flag #t)(set! cnt (1+ cnt))
				   (set! words (append words (list (car st))))
				   (sparse-add! vec (list-ref st nnvec)))))))
			(let ((w (lookup-word wrd)))
			  (if (and w (list-ref w nnvec))
			      (begin
				(set! flag #t)(set! cnt (1+ cnt))
				(set! words (append words (list (car w))))
				(sparse-add! vec (list-ref w nnvec))))))))
	      wordlist)
	     (display time out)(display " " out)
	     (display label out)(display " " out)
	     (display cnt out)(display " " out)
	     (if flag
		 (begin
		   (for-each-unique-word
		    (lambda (target)
		      (if (and (not (member (car target) words))
			       (list-ref target nnvec)
			       (not (member (car target) STOP))
			       (>= (list-ref target freq) MINFREQ)
			       (not (string-prefix? "###" (car target)))
			       (not (string-prefix? "***" (car target))))
			  (set! ret (cons (list (car target)
						(sparse-corr (list-ref target nnvec)
							     vec))
					  ret)))))
		   (for-each (lambda (r)
			       (display (car r) out) (display " " out)
			       (let ((nr (number->string (cadr r))))
				 (if (> (string-length nr) 5)
				     (display (substring nr 0 5) out)
				     (display nr out))
				 (display " " out)))
			     (list-head (sort ret (lambda (x1 x2)
						    (> (cadr x1) (cadr x2))))
					NR_ASS))
		   (newline out))
		 (display "#f\n" out)))
	   (set! line (read-line in)))))

(define (make-compositional polehash file)
  (let* ((out (open-file OUTFILE2 "a"))
	 (in (open-input-file file))
	 (line (read-line in)))
    (while (not (eof-object? line))
	   (let* ((wrds (string-split line #\sp))
		  (id (car wrds))
		  (docvec (make-dense VEC)))
	     (display id out)
	     (for-each 
	      (lambda (w)
		(if (and (not (member w STOP))
			 (lookup-word w)
			 (get-dir_ctx (lookup-word w)))
		    (sparse-add-scaled! docvec (get-dir_ctx (lookup-word w))
					(weight (get-freq (lookup-word w)) 0)))
); lambda
	      (cdr wrds))
	     (if (empty? docvec)
		 (display " #f" out)
		 (hash-for-each
		  (lambda (pk pv)
		    (display " " out) (display pk out) (display " " out)
		    (if pv
			(display (sparse-corr docvec pv) out)
			(display "#f" out)))
		  polehash))
	     (newline out))
	   (set! line (read-line in))	   );while
	   (close-port in)
	   (close-port out)
    ))


(define (make-compositional-string polehash file)
  (let* ((in (open-input-file file))
	 (out "")
	 (line (read-line in)))
    (while (not (eof-object? line))
	   (let* ((wrds (string-split line #\sp))
		  (id (car wrds))
		  (docvec (make-dense VEC)))
	     (if (not (string-null? id))
(begin
	     (for-each 
	      (lambda (w)
		(if (and (not (member w STOP))
			 (lookup-word w)
			 (get-dir_ctx (lookup-word w)))
;		    (sparse-add-scaled! docvec (sparse-prod (get-dir_ctx (lookup-word w)) (get-dir_ctx (lookup-word w)))
		    (sparse-add-scaled! docvec (get-dir_ctx (lookup-word w))
					(weight (get-freq (lookup-word w)) 0)))
		); lambda
	      (cdr wrds)
	      ); f-e
	     (set! out (string-append "id=" id))
	     (hash-for-each
		  (lambda (pk pv)
		    (set! out (string-append out "&" pk "="))
		     (if (empty? docvec)
			 (set! out (string-append out "0.00"))
			 (if pv
			     (set! out (string-append out (number->string (sparse-corr docvec pv))))
			     (set! out (string-append out "0.0")) ) ) )
		  polehash) ; h-f-e
) ) ; if
	     (set! line (read-line in))
	     ); let*
	   );while
    (close-port in)
    out
    ); let*
); define

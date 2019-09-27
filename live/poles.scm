; poles.scm - proprietary to gavagai ab
; www.gavagai.se
;--------------------------------------------------------
; * POLES MODULE
; functions for setting up poles
;--------------------------------------------------------
; * REVISION HISTORY
; 2010-05-05 MS+JK: setup-poleword-hash defined
; 2009-08-07 MS: functions finalized and moved to module
;--------------------------------------------------------
; help function for (check...)
; find nearest neighbors and return a list of vectors
(define (extract-nns wrd compvec retvec)
  (let ((res '()))
    (for-each-unique-word
     (lambda (wrd2)
       (if (and (not (eq? wrd2 wrd))
		(list-ref wrd2 compvec)
		(list-ref wrd2 retvec)
		(>= (list-ref wrd2 freq) MINFREQ)
		(not (string-prefix? "###" (car wrd2))))
	   (set! res (cons (list (list-ref wrd2 retvec)
				 (sparse-corr (list-ref wrd2 compvec) (list-ref wrd compvec)))
			   res)))))
    (list-head (sort res (lambda (x1 x2) (> (cadr x1) (cadr x2)))) NR_NNS)))

; help function for (setup-poles ...)
; check if the word is ok and if so add its vector
; and its NR_NNS nns vectors
(define (check word vec addvec nnvec flag)
  (if (and (not (member (car word) STOP))
	   (list-ref word addvec))
      (begin
	(if flag (set-targetword word)) ; targetword
	(sparse-add! vec (list-ref word addvec))
	(if (and NR_NNS nnvec (list-ref word nnvec))
	    (let ((nns (extract-nns word nnvec addvec)))
	      (for-each (lambda (nn)
			  (sparse-add-scaled! vec (car nn) (cadr nn)))
			nns))))))

; setup function for both poles and targets
; makes vectors for the poles specified in FILE
; (with format: pole-name wrd wrd wrd wrd ...\n)
; format for the pole hash is: pole-name vector
; flag is whether we should mark the words as targetwords
(define (setup-poles type FILE vector nnvector flag)
  (let* ((in (open-input-file FILE))
	 (line (read-line in)))
    (while (not (eof-object? line))
	   (let* ((lst (string-split line #\sp))
		  (name (car lst)) (wrds (cdr lst))
		  (vek (make-dense VEC)))
	     (for-each 
	      (lambda (wrd)
		(if (string-prefix? "*" wrd)
		    (let ((stem (string-drop wrd 1)))
		      (if (string-suffix? "*" stem)
			  (let ((infix (string-drop-right stem 1)))
			    (for-each-unique-word
			     (lambda (st)
			       (if (string-contains (car st) infix)
				   (check st vek vector nnvector flag)))))
			  (for-each-unique-word
			   (lambda (st)
			     (if (string-suffix? stem (car st))
				 (check st vek vector nnvector flag))))))
		    (if (string-suffix? "*" wrd)
			(let ((prefix (string-drop-right wrd 1)))
			  (for-each-unique-word
			   (lambda (st)
			     (if (string-prefix? prefix (car st))
				 (check st vek vector nnvector flag)))))
			(let ((w (lookup-word wrd)))
			  (if w (check w vek vector nnvector flag))))))
	      wrds)
	     (if (empty? vek) ; from vector.scm
		 (hash-set! type name #f)
		 (hash-set! type name vek))) ; normalize the vector first?
	   (set! line (read-line in)))
    (close-port in)))

; populates a list with the pole words in FILE and returns it
(define (setup-polewords FILE)
  (let* ((in (open-input-file FILE))
         (line (read-line in))
	 (res '()))
    (while (not (eof-object? line))
	   (for-each (lambda (w)
		       (set! res (append res (list w))))
		     (cdr (string-split line #\sp)))
	   (set! line (read-line in)))
    res))

(define (setup-poleword-hash FILE)
  (let* ((in (open-input-file FILE))
         (line (read-line in))
	 (res (make-hash-table)))
    (while (not (eof-object? line))
	   (let ((l (string-split line #\sp)))
	     (hash-set! res (car l) (cdr l)))
	   (set! line (read-line in)))
    res))


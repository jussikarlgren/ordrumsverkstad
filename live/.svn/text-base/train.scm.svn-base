; train.scm - proprietary to gavagai ab
; www.gavagai.se
;--------------------------------------------------------
; * TRAINING MODULE
; functions for accumulating context vectors
;--------------------------------------------------------
; * REVISION HISTORY
; 2010-01-10 JK: added variable FORGET 
; 2010-01-10 JK: added catch for freq=#f in (weight x x)
; 2009-10-26 MS: removed ctx2repr
; 2009-10-21 MS: changed the K variable in (weight...)
; 2009-10-13 MS: compositional functions moved to the anal
; 2009-08-07 MS: smoking out bugs
; 2009-06-11 MS: complete rewrite
;--------------------------------------------------------
; incremental frequency weighter
; K=10 (or more - the higher the more aggressive cutoff; 5 is good for TASA, but blogdata is likely to contain more high-frequency noise)
; A=0.2 (seems like a good choice)
; 1+ to avoid division with zero prob for first word, scale to handle words with not updated freq
(define (weight freq scale)
  (if freq
      (let ((w (expt (+ freq scale)
		     (- (* KKK (/ (+ freq scale)
				  (* (1+ nr_words) 0.2)))))))
	(if (> w 0.75)
	    1.0
	    w))
      1))

; accumulate context vectors and increment frequencies
(define (train)
  (let ((doccode #f)
        (word (get-word the_text nr_words))
	(dochash (make-hash-table))
	(dirhash (make-hash-table)))
    (while word
	   (if (string=? (car word) "***")
	       (begin 
		 (set! nr_docs (1+ nr_docs))
		 (set! doccode #f))
	       (begin
		 (if DOC
		     (if (get-doc_ctx word)
			 (begin
			   (if (not doccode)
			       (set! doccode (encode (number->string nr_docs))))
			   (if FORGET
			       (if (hash-ref dochash word)
				   (sparse-add! (hash-ref dochash word) doccode)
				   (hash-set! dochash word
					      (sparse-add! (make-dense VEC) doccode)))
			       (sparse-add! (get-doc_ctx word) doccode)))))
		 (if DIR
		     (if (get-dir_ctx word)
			 (add-ctx-window DIR_WIN word dir_ctx repr 1 dirhash)))))
	   (set! nr_words (1+ nr_words))
	   (increment-freq word)
	   (set! word (get-word the_text nr_words)))
    (if FORGET
	(begin
	  (if DOC (scale-and-forget dochash doc_ctx))
	  (if DIR (scale-and-forget dirhash dir_ctx))))))
;    (if DIR (ctx2repr dir_ctx)))) ; sparsify ctx and copy to repr

; add vectors from words in a context window to the ctx vector for word
(define (add-ctx-window nr wrd wrdvec addvec rot tmphash)
  (let ((i 1)
	(addwrd '())
	(flag #t)
	(tmpvec (make-dense VEC)))
    (while (<= i nr)
	   (set! addwrd (get-word the_text (- nr_words i)))
	   (if addwrd
	       (if (equal? "***" (car addwrd)) ; dont cross doc boundaries
		   (set! flag #f)))
	   (if (and flag addwrd (list-ref addwrd addvec))
	       (if FORGET
		   (sparse-add-scaled!
		    tmpvec (dense-shift (list-ref addwrd addvec) (- rot))
		    (weight (get-freq addwrd) 0)) ; 0 cause freq is ok
		   (sparse-add-scaled!
		    (list-ref wrd wrdvec)
		    (dense-shift (list-ref addwrd addvec) (- rot))
		    (weight (get-freq addwrd) 0)))) ; 0 cause freq is ok
	   (set! i (1+ i)))
    (set! i 1)
    (set! addwrd '())
    (set! flag #t)
    (while (<= i nr)
	   (set! addwrd (get-word the_text (+ nr_words i)))
	   (if addwrd
	       (if (equal? "***" (car addwrd)) ; dont cross doc boundaries
		   (set! flag #f)))
	   (if (and flag addwrd (list-ref addwrd addvec))
	       (if FORGET
		   (sparse-add-scaled!
		    tmpvec (dense-shift (list-ref addwrd addvec) (+ rot))
		    (weight (get-freq addwrd) 1)) ; 1 to increment freq
		   (sparse-add-scaled!
		    (list-ref wrd wrdvec)
		    (dense-shift (list-ref addwrd addvec) (+ rot))
		    (weight (get-freq addwrd) 1)))) ; 1 to increment freq
	   (set! i (1+ i)))
    (if FORGET
	(if (hash-ref tmphash wrd)
	    (sparse-add! (hash-ref tmphash wrd) tmpvec)
	    (hash-set! tmphash wrd tmpvec)))))

; forget unprocessed words and scale processed ones
(define (scale-and-forget hashtab vek)
;   (let ((keys (map (lambda (e) (car (car e))) (hash-fold acons '() hashtab))))
;     (for-each-unique-word (lambda (ele)
; 			    (if (and (not (member (car ele) keys))
; 				     (not (empty? (list-ref ele vek))))
; 				(list-set! ele vek (sparse-threshold (list-ref ele vek) (/ (high (list-ref ele vek)) 3))))))) ; FORGET BY THRESHOLDING
  (hash-for-each (lambda (k v)
		   (if (not (empty? (list-ref k vek)))
		       (sparse-add-scaled! v (list-ref k vek) FORGET)) ; SCALE FACTOR
		   (list-set! k vek v))
		 hashtab))

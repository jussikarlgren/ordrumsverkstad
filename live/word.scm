; word.scm - proprietary to gavagai ab
; www.gavagai.se
;--------------------------------------------------------
; * WORD MODULE
; functions for setting, accessing and modifying 
; the word features
;--------------------------------------------------------
; * USAGE NOTES:
; To use minimal reps: redefine the nr_slots variable + 
; the relevant vector pointers (e.g. repr, dir_ctx, freq)
; in the .init file
;--------------------------------------------------------
; * REVISION HISTORY
; 2010-05-05 JK+MS: new hedge and negation query predicates
; 2010-02-24 MS: added one slot in the word reps
;                for marking target words
; 2009-06-10 MS: word functions revised
;                and moved to separate file 
;--------------------------------------------------------
; * TODO
; add df slot?
;--------------------------------------------------------

; nr of slots in the word lists
(define nr_slots 5)

; index vector
(define repr 1)
(define (get-repr w)
  (list-ref w repr))
(define (set-repr w)
  (list-set! w repr (encode (car w))))
(define (delete-repr w)
  (list-set! w repr #f))

; doctrain for associations
(define doc_ctx 2)
(define (get-doc_ctx w)
  (list-ref w doc_ctx))
(define (set-doc_ctx w vek)
  (list-set! w doc_ctx vek))
(define (delete-doc_ctx w)
  (list-set! w doc_ctx #f))

; dirtrain for dense directional vectors
(define dir_ctx 3)
(define (get-dir_ctx w)
  (list-ref w dir_ctx))
(define (set-dir_ctx w vek)
  (list-set! w dir_ctx vek))
(define (delete-dir_ctx w)
  (list-set! w dir_ctx #f))

; frequency
(define freq 4)
(define get-freq (lambda (w)
		   (if w (list-ref w freq)
		       0)))
(define (set-freq w f)
  (list-set! w freq f))
(define (increment-freq w)
  (set-freq w (1+ (get-freq w))))
(define (decrement-freq w)
  (set-freq w (1- (get-freq w))))

; targetword
(define (targetword? w)
  (list-ref w 5))

(define (set-targetword w)
  (list-set! w 5 #t))

; copy a sparsified version of ctx to repr
(define (ctx2repr ctx)
  (for-each-unique-word 
   (lambda (wrd)
     (list-set! wrd repr 
		(sparse-threshold (list-ref wrd ctx)
				  (/ (high (list-ref wrd ctx)) 3))))))
 
; client specific context vector setter
(define (set-contexts wrd)
  (if DOC
      (set-doc_ctx wrd (make-dense VEC)))
  (if DIR
      (set-dir_ctx wrd (make-dense VEC))))

; make index and context vectors for new words
(define (register-new-words)
  (for-each-unique-word 
   (lambda (ele)
     (if (null? (cdr ele))
	 (begin
	   (set-cdr! ele (make-list nr_slots #f))
	   (set-repr ele)
	   (set-contexts ele)
	   (set-freq ele 0))))))

; make index and context vectors for all words
(define (register-all-words)
  (for-each-unique-word 
   (lambda (ele)
     (set-cdr! ele (make-list nr_slots #f))
     (set-repr ele)
     (set-contexts ele)
     (set-freq ele 0))))

(define NULLWORD (make-list nr_slots))

; is the word ok to use? prime can be the NULLWORD
(define (good-word? word vek prime)
  (if (and word (not (eq? (cdr word) (cdr prime)))
	   (list-ref word vek)
	   (not (member (car word) STOP))
	   (>= (list-ref word freq) MINFREQ)
	   (not (string-prefix? "###" (car word)))
	   (not (string-prefix? "***" (car word))))
      #t
      #f))

; is the word a negation?
(define (negation? word)
  (member word NEGATION))

; is the word a hedge?
(define (hedge? word)
  (member word HEDGELIST))

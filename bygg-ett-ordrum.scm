; we dont need warnings about duplicate bindings
(default-duplicate-binding-handler 'last)
; load some useful modules
(use-modules (ice-9 rdelim)
	     (srfi srfi-1)
	     ((srfi srfi-19) :renamer (symbol-prefix-proc 'tm:)))

;========= project specific paths, sockets, labels etc
(define PROJECT "/home/jussi/projekt/ordrumsverkstad/")
;(define DATADIRECTORY "/home/jussi/projekt/2019.svsprlabb/ordrumsverkstad/")
(define DATADIRECTORY "/repository/data/wikipedia/sv/")
;(define infile "liten");
(define infile (string-append DATADIRECTORY "svwikiclean.txt"))
(define LABEL "wiki")
(display (string-append "processing " infile))
(newline)
;========= for vector.scm
; vector length
(define VEC 1000)
; sparseness
(define SPARSE 3)
(define SEED 19651127)
; initialize the encoder
(set-seed SEED)
(init-encoder 100 VEC SPARSE)
;============
; config for dirtrain
(define DIRECTION #t); normally, #t for directional differentiation
;============
; window size for context windows
(define WIN 2)
;============
; config for dirtrain
(define DOC #f)
;=======  outfiles 
; 1 target tracking
; 2 document tracking
; 3 paradigms
; 4 associations
; 5 wordspaces
(define OUTFILE1 #f) ; (string-append PROJECT LABEL ".targets.out"))
(define OUTFILE2 #f) ; (string-append PROJECT "gavagai-" LABEL ".docs.out"))
(define OUTFILE3 #f) ; 
(define OUTFILE4 #f) ; 
(define OUTFILE5 (string-append PROJECT "gavagai-" LABEL ".wordspace"))
(define LOGFILE (string-append PROJECT "gavagai-" LABEL ".log"))


(define (run-file infile) 
  (begin 
    (train-file infile DIRECTION (1+ WIN) DOC) ;; add one to window for starting index snafu purposes
    (if (defined? (quote OUTFILE5))
	(let (
	      (scotch (open-file LOGFILE "a")))
	  (display (string-append "exporting words to " OUTFILE5) scotch)
	  (export-words OUTFILE5)
	  (newline scotch)
	  (close scotch)
	);let
	);if outifle5
    )
)

(define (train-file infile dir win doc)
  (if (> win 0) (display (string-append "Context window training\n")))
  (if (and (> win 0) dir) (display "Directional windows\n"))
  (if doc (display "Document contexts\n"))
  (display "Started: ")(display (time))
  (let* ((in (open-input-file infile))
	 (line (read-line in))
	 (nr_docs 0))
    (while (not (eof-object? line))
	   (set! nr_docs (1+ nr_docs))
	   (if (not (equal? "" line))
	       (let ((lst (delete "" (string-split line #\sp))))
		 (display (time)) (display " ") (display nr_docs) (newline)
		 (if (> win 0) (train-win lst win dir))
		 (if doc (train-document lst nr_docs))
		 ))
	   (set! line (read-line in))))
  (display nr_words) (display " words processed\n")
  (display "Finished: ")(display (time)))


(define WEIGHTCONSTANT 20)
(define (weight freq) 1)
 ; standard-weight from code base
(define (standard-weight freq)
  (if freq
	(exp (- (* WEIGHTCONSTANT (/ freq nr_types))))
	0.0))


; proprietary frequency-weighting
; designed by aho/ms
(define KKK 5)
(define LLL 0.2)
(define TTT 0.75)
(define (prop-weight freq scale)
  (if freq
      (let ((w (expt (+ freq scale)
		     (- (* KKK (/ (+ freq scale) 
				(* (1+ nr_words) LLL))))))) 
;;	w)
	(if (> w TTT)
	    1.0
	    w)) ;0.0
      1.0))

; global variables - bad habit, but hey, it's guile!
(define nr_words 0)
(define nr_types 0)

; what's the time?
(define (time)
  (string-append (tm:date->string (tm:time-utc->date (tm:current-time))) "\n"))

; rotate a sparse vector
(define (sparse-shift vec num)
  (sparse-shift! (sparse-add! (make-sparse (sparse-length vec)) vec) num))

; train context vectors 
(define (train-win sentlist win dir)
  (let ((len (length sentlist))
	(ind 0))
    (for-each
     (lambda (o)
       (let* ((k (string-downcase (string-copy o)))
	      (ele (lookup-word k)) 
	      (c 1))
	 (if ele (increment-freq ele)
	     (begin
	       (create-new-word k 1)
	       (set! ele (lookup-word k))))
	 (while (< c win)
		(if (< (+ ind c) len)
		    (let* ((kk (string-downcase (string-copy (list-ref sentlist (+ ind c)))))
			   (dep (lookup-word kk)))
		      (if (not dep)
			  (begin
			    (create-new-word kk 0)
			    (set! dep (lookup-word kk))))
   		      (sparse-add-scaled! (get-dir_ctx ele) (if dir (sparse-shift (get-repr dep) +1) (get-repr dep)) (standard-weight (+ 1 (get-freq dep))))
   		      (sparse-add-scaled! (get-dir_ctx dep) (if dir (sparse-shift (get-repr ele) -1) (get-repr ele)) (standard-weight (get-freq ele)))
 		      (set! c (1+ c)))
		    (set! c win)))
	 (set! ind (1+ ind))
	 (set! nr_words (1+ nr_words))))
     sentlist)))

; train document vectors
(define (train-document sentlist nr_docs)
  (let* (
	 (len (length sentlist))
	 (ind 0)
	 (doccode (encode (number->string nr_docs)))
	 )
;    (display nr_docs) (display " ") (display sentlist) (display " ") (display doccode) (newline)
    (for-each
     (lambda (o)
       (let* ((k (string-downcase (string-copy o)))
	      (ele (lookup-word k)) )
	 (if ele (if (not (> WIN 0)) (increment-freq ele))
	     (begin
	       (create-new-word k 1)
	       (set! ele (lookup-word k))))
	 (sparse-add-scaled! (get-doc_ctx ele) doccode 1)
	 (set! ind (1+ ind))
	 (set! nr_words (1+ nr_words))))
     sentlist)))
;=========================================================================================
; word object
(define (create-new-word k freq) 
  ; (display nr_types) (display "+") (display k) 
  (set! nr_types (+ 1 nr_types)) 
  ; (display "->") (display nr_types) (newline) 
  (create-word k (encode k) (make-dense VEC) (make-dense VEC) freq))

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

;=========================================================================================
(run-file infile)

(define probes '("alice" "queen" "red" "wish" "ran"))
(define topscore (make-hash-table))
(for-each (lambda (probeword)
	    (hash-set! topscore probeword 0))
	    probes)

(for-each-unique-word
 (lambda (e)
   (let ((w (car e)) (ctxw (get-dir_ctx e)))
     (if (and
	  (> (string-length w) 1)
	  (> (get-freq e) 5))
	 (for-each (lambda (probeword)
	     (let* ((ctxv (get-dir_ctx (lookup-word probeword))) (c (sparse-corr ctxw ctxv))
		    )
	       (if (and (not (string= probeword w)) (> c (hash-ref topscore probeword)))
		   (begin 
		     (hash-set! topscore probeword c)
		     (display w) (display " ") (display probeword) (display " ") (display c) (display " ") (display (get-freq e)) (newline)
		     )
		   )
	       )
	     )
		   probes)
	 )
     )))

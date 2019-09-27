; core.scm - proprietary to gavagai ab
; www.gavagai.se
;--------------------------------------------------------
; * CORE MODULE
; core functions for gavagai analysis
;--------------------------------------------------------
; * REVISION HISTORY
; 2010-05-12 MS: moved target hash inside the main loop,
;                deleted global target and pole hashes
; 2010-05-05 JK+MS: entirely new compositionality functionality with hitherto unknown ramifications
; 2010-04-10 JK: new file name standard imported from tsup development project
; 2009-10-13 MS: compositional functions re-conceptualized
; 2009-08-07 MS: re-write completed
; 2009-06-12 MS: core functions moved to core module
;--------------------------------------------------------
; import modules
(use-modules (ice-9 popen)
             (ice-9 regex)
             (ice-9 readline)
             (ice-9 rdelim)
	     (srfi srfi-19))

; import client specific global variables
; if they are given on the command line
(let ((command (command-line)))
  (if (> (length command) 1) 
      (load (cadr command))))

;--------------------------------------------------------
; GLOBAL VARIABLES
;--------------------------------------------------------
; global gsdm text object
(define the_text #f)

; counter for how many words we have processed
(define nr_words 0)

; counter for how many documents we have processed
(define nr_docs 0)

; global memory for processed data
; filename -> ((startwrdpos endwrdpos) . (startdocnr enddocnr))
; wordpos: (car (hash-ref seen FILE))
; docpos: (cdr (hash-ref seen FILE))
(define seen (make-hash-table))

;--------------------------------------------------------
; SET UP THE REPRESENTATION
;--------------------------------------------------------
; add new text to the word space
(define (add-new-text file)
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
	(register-new-words))))

;--------------------------------------------------------
; EAT FILES IN A DIRECTORY AND RABBITIZE THEM
;--------------------------------------------------------
; the main loop down the rabbit hole
(define (main-loop)
  (let* ((dir (opendir DATA))
	 (f (readdir dir))
	 (sorted '()))
    (while (not (eof-object? f))
 	   (if (string-suffix? ".txt" f)
	       (set! sorted 
		     (append sorted 
			     (list 
			      (date->time-utc 
			       (string->date 
; substring starting from poisiton 8 is 'a bit' hacky...
; better to use 0 and require data files to have file names according to:
; year-month-day.txt
				(substring f 8 (string-contains f ".txt"))
				"~Y-~m-~d"))))))
	   (set! f (readdir dir)))
    (closedir dir)
    (for-each (lambda (ele)
		(let* ((date (date->string (time-utc->date ele) "~Y-~m-~d"))
; ouch this is ugly as fuck! again, use a consistent file name format
		       (file (string-append 
			      "twingly."
			      (regexp-substitute/global #f "-0" date 'pre "-" 'post)
			      ".txt")))
		  (if (not (hash-ref seen file))
		      (let ((target_hash (make-hash-table)))
			(hash-set! seen file (cons (list nr_words) (list nr_docs)))
; borde trådas
			(add-new-text (string-append DATA file))
			(train)
; borde oxå trådas
			(if POLES
; filen i *.chunk innehåller alla ord med MÅLORD som första ord
			    (let* ((in (open-input-file
					(string-append CHUNKS file ".chunk")))
				   (line (read-line in))
				   (polehash (setup-poleword-hash POLES))
				   (out (open-file OUTFILE1 "a")))
			      (while
			       (not (eof-object? line))
			       (let* ((wrdlst (string-split line #\sp))
				      (thistarget (hash-ref target_hash (car wrdlst))))
				 (hash-for-each
				  (lambda (k v)
				    (let* ((ny (make-compositional-new
						(cdr wrdlst) v)))
				      (if thistarget
					  (if (hash-ref thistarget k)
					      (hash-set!
					       thistarget
					       k (+ (hash-ref thistarget k) ny))
					      (hash-set! thistarget k ny))
					  (begin
					    (hash-set! target_hash
						       (car wrdlst)
						       (make-hash-table))
					    (hash-set! (hash-ref target_hash
								 (car wrdlst))
						       k ny)))))
				  polehash))
			       (set! line (read-line in)))
			      (hash-for-each
			       (lambda (k v)
				 (display date out)(display " " out)
				 (display k out)(display " " out)
				 (hash-for-each (lambda (v_k v_v)
						  (display v_k out)(display " " out)
						  (display v_v out)(display " " out))
						v)
				 (newline out))
			       target_hash)
			      (close-port in)(close-port out)))
			(if TARGETS
			    (let ((targetwords (setup-polewords TARGETS))
; 				  (target_syns (make-hash-table))
				  (target_ass (make-hash-table)))
			      (if PARA
; 				  (begin
;                                     (setup-poles target_syns TARGETS dir_ctx #f #t)
				  (associate targetwords dir_ctx date OUTFILE3))
			      (if ASS
				  (begin
                                    (setup-poles target_ass TARGETS doc_ctx #f #t)
				    (associate-poles target_ass doc_ctx date OUTFILE4)))))
			(hash-set! seen file (cons 
					      (append (car (hash-ref seen file))
						      (list nr_words))
					      (append (cdr (hash-ref seen file))
						      (list nr_docs))))))))
	      (sort sorted (lambda (x1 x2) (time<? x1 x2)))))
;  (sleep SEC)
;  (if (> SEC 1) (main-loop))
)



(define (gavagai)
  (if BGDATA (import-words BGDATA))
  (main-loop))

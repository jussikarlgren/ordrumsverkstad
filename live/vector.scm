; vector.scm - proprietary to gavagai ab
; www.gavagai.se
;--------------------------------------------------------
; * VECTOR MODULE
; functions for vector operations
;--------------------------------------------------------
; * REVISION HISTORY
; 2009-06-11 MS: vector functions moved to module
;--------------------------------------------------------
; * TODO
; make forget function that blurs a vector with a 
; centroid/expectancy vector
;--------------------------------------------------------

; check if a vector is empty
(define (empty? vek)
  (= 0.0 (sparse-prod vek vek)))

; rotate a sparse vector
; + to the right, - to the left
(define (sparse-shift vek num)
  (sparse-shift! 
   (sparse-add! 
    (make-sparse 
     (sparse-length vek)) 
    vek)
   num))

; rotate a dense vector
; + to the right, - to the left
(define (dense-shift vek num)
  (sparse-shift! 
   (sparse-add! 
    (make-dense 
     (sparse-length vek)) 
    vek) 
   num))

; densify vector v
(define (densify v)
  (sparse-add! (make-dense (sparse-length v))
               v))

; orthogonalize yesvec w.r.t. novec (from widdows acl'03)
(define (negate yesvec novec)
  (let* ((normyes (make-dense VEC))
	 (normno (make-dense VEC)))
    (set! normyes (sparse-add! normyes yesvec))
    (set! normno (sparse-add! normno novec))
    (let ((prod (sparse-prod normyes normno))
	  (noprod (sparse-prod normno normno)))
      (sparse-add-scaled! normyes
			  (sparse-scale! normno (/ prod noprod))
			  -1))
    normyes))

; identify the highest value in vec
(define (high vek)
  (let ((stop (sparse-length vek))
	(ind 0)
	(res '()))
    (while (< ind stop)
	   (let ((v (sparse-ref vek ind)))
	     (if (eq? '() res)
		 (set! res (list v))
		 (if (> v (car res))
		     (set! res (list v)))))
	   (set! ind (1+ ind)))
    (car res)))

; forget by thresholding vec to a sparse vector
(define (forget vek)
  (for-each-unique-word 
   (lambda (wrd)
	 (let ((v (list-ref wrd vek)))
	   (if (and v (not (empty? v)))
	       (list-set! wrd vek (sparse-threshold v (/ (high v) 2))))))))

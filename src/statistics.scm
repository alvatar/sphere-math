;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistical procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Statistics
;-------------------------------------------------------------------------------

;;; Takes the smallest value of a list

(define (pick-min l) (apply min l))

;;; Takes the biggest value of a list

(define (pick-max l) (apply max l))

;;;

(define (mode l)
  (error "Not implemented"))

;;;

(define (running-mode! l)
  (error "Not implemented"))

;;; 1 arg: mean of all values in a list
;;; 2 args: mean of the two values

(define arithmetic-mean ; TODO: Check if optimizes, build it from specific mean2/mean-list generators, so client can choose
  (case-lambda
   ((l) (/ (sum l) (length l))) ; OPTIMIZE
   ((a b) (/ (+ a b) 2))))

;;; 

(define (running-arithmetic-mean!)
  (error "Not implemented"))

;;;

(define (geometric-mean l)
  (error "Not implemented"))

;;;

(define (running-geometric-mean! l)
  (error "Not implemented"))

;;;

(define (harmonic-mean list)
  (/ (let loop ((counter 0) (l list))
			(cond
				((null? l) counter)
				(else
					(loop (+ counter 1) (cdr l)))))
		(let loop ((l list))
			(cond
				((null? l) 0)
				(else
					(+ (/ 1 (car l))
						(loop (cdr l))))))))

(print(harmonic-mean '(1 2 4)))

;;;

(define (running-harmonic-mean! l)
  (error "Not implemented"))

;;; Weighed mean of the values of the first list, the second are the weights

(define (weighted-mean vl wl)
  (if (not (= (length vl) (length wl)))
      (error "Values and weights lists are not of the same length")) ; TODO: Change to arg-checks
  (let ((q (fold (lambda (v w num.den) (list
                                   (+ (* v w) (car num.den))
                                   (+ w (cadr num.den))))
                 '(0 0) vl wl)))
    (/ (car q) (cadr q))))

;;;

(define (running-weighted-mean! vl wl)
  (error "Not implemented"))

;;;

(define (quadratic-mean list)
	(sqrt (* (/ 1 
				(let loop ((counter 0) (l list))
					(cond 
						((null? l) counter)
						(else
							(loop (+ counter 1) (cdr l))))))
			(apply + (map (lambda(x)(* x x)) list)))))


(print (quadratic-mean '(1 2 3 4 5)))

;;;

(define (running-quadratic-mean! l)
  (error "Not implemented"))

;;;

(define (mid-range l)
  (error "Not implemented"))

;;;

(define (running-mid-range! l)
  (error "Not implemented"))

;;;

(define (standard-deviation l)
  (error "Not implemented"))

;;;

(define (running-standard-deviation! l)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Probability and sampling
;-------------------------------------------------------------------------------

;;;

(define (ziggurat f x)
  (error "Not implemented"))

;;;

(define (ratio-of-uniforms f x)
  (error "Not implemented"))
  
;;; Inverted cumulative distribution function

(define (inverted-cdf f x)
  (error "Not implemented"))

;;; Marsaglia polar method

(define (marsaglia f x)
  (error "Not implemented"))


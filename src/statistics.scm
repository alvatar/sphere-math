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

;;; Mode
(define (mode l)
  (error "Not implemented"))

;;; Mode with list memory
(define (running-mode! l)
  (error "Not implemented"))

;;; Arithmetic mean
(define (arithmetic-mean l)
  (let recur ((accum 0)
              (n 0)
              (l l))
    (if (null? l)
        (/ accum n)
        (recur (+ accum (car l))
               (++ n)
               (cdr l)))))
(define (running-arithmetic-mean!)
  (error "Not implemented"))

;;; Arithmetic mean of two values
(define (arithmetic-mean/2 a b)
  (/ (+ a b) 2))

;;; Geometric mean
(define (geometric-mean l)
  (expt (apply * l) (/ 1 (length l))))
(define (running-geometric-mean! l)
  (error "Not implemented"))

;;; Harmonic mean
(define (harmonic-mean l)
  (let recur ((accum 0)
              (n 0)
              (l l))
    (if (null? l)
        (/ n accum)
        (recur (+ (/ 1 (car l)) accum)
               (++ n)
               (cdr l)))))
(define (running-harmonic-mean! l)
  (error "Not implemented"))

;;; Harmonic mean of two values
(define (harmonic-mean/2 a b)
  (/ (* 2 a b)
     (+ a b)))

;;; Weighed mean of the values of the first list, the second list are the weights
(define (weighted-mean vl wl)
  (let recur ((vl vl)
              (wl wl)
              (num 0)
              (denom 0))
    (if (or (null? vl) (null? wl))
        (/ num denom)
        (recur (cdr vl)
               (cdr wl)
               (+ (* (car vl)
                     (car wl))
                  num)
               (+ (car wl)
                  denom)))))
(define (running-weighted-mean! vl wl)
  (error "Not implemented"))

;;; Quadratic mean
(define (quadratic-mean l)
  (let recur ((accum 0)
              (n 0)
              (l l))
    (if (null? l)
        (sqrt (/ accum n))
        (recur (+ ((lambda (x) (* x x)) (car l)) accum)
               (++ n)
               (cdr l)))))
(define (running-quadratic-mean! l)
  (error "Not implemented"))

;;; Mid range
(define (mid-range list)
  (/ (+ (apply max list) (apply min list))
     2))
(define (running-mid-range! l)
  (error "Not implemented"))

;;; Variance
(define (variance l)
  (let ((mean (arithmetic-mean l)))
    (let recur ((accum 0)
                (n 0)
                (l l))
      (if (null? l)
          (/ accum n)
          (recur (+ ((lambda (x) (* x x)) (- (car l) mean))
                    accum)
                 (++ n)
                 (cdr l))))))
(define (running-standard-deviation! l)
  (error "Not implemented"))

;;; Standard deviation
(define (standard-deviation l)
  (sqrt (variance l)))

;;; Standard deviation with list memory
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


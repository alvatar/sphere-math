;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedures for numerical interpolation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;-------------------------------------------------------------------------------
; Integer interpolation
;-------------------------------------------------------------------------------
;;; '(0 4 8 6 2 5) -> '(0 1 2 3 4 5 6 7 8)

(define (range-expand l)
	(let loop ((m (apply min l))
  			   (M (apply max l))
  			   (list '()))
		(cond 
			((= m M) (reverse (cons m list)))
			(else
				(loop (+ m 1) M (cons m list))))))

;;(print (range-expand '(0 4 8 6 2 5)))


;;; '(0 4 8 6 2 5) -> '(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 3 4 5)

(define (full-range-expand l)
	(let ((list (range-expand l)))
		(append list (cdr (reverse list)))))

;;(print (full-range-expand '(0 4 8 6 2 5)))
;;; '(0 1 2 3 4 5 6 7 8 7 6 5 4 3 2 3 4 5) -> (0 8 2 5)

(define (range-extract l)
  (error "not implemented"))

;-------------------------------------------------------------------------------
; Uni-dimensional interpolation
;-------------------------------------------------------------------------------

(define (interpolate/nearest l)
  (error "Not implemented"))

;;;

(define (interpolate/linear l)
  (error "Not implemented"))

;;;

(define (interpolate/polynomial l)
  (error "Not implemented"))

;;; Support B-splie, Bézier spline, research others

(define (interpolate/spline l method)
  (error "Not implemented"))

;;;

(define (interpolate/hermite l)
  (error "Not implemented"))

;;; Notes:
;;;
;;; Consider making function generators, so the created function retains the dataset
;;; avoiding computation
;;;
;;; Multi-variate interpolation
;;; Kriging?
;;; Extrapolation?
;;; (define (interpolate/whittaker-shannon l)


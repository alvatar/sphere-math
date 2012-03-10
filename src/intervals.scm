;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Intervals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Clamp value between low and high values

(define (clamp x lo hi)
  (cond ((< x lo) lo)
        ((> x hi) hi)
        (else x)))

;;; Normalize value in a range

(define (normalize x lo hi)
  #;
  (%accept (and (> x lo)
                (< x hi))
           "value is outside normalization boundaries")
  (/ (- x lo) (- hi lo)))

;;; Takes a value and two boundaries, using any as reference, inverts the intervals

(define (invert x lo hi) ; TODO: should check boundaries (or clamp)?
  (+ lo (- hi x)))

;;; Clamp value and normalize it to the given boundaries

(define (clamp&normalize x lo hi) ; TODO: try with compose
  (normalize (clamp x lo hi) lo hi))

;;; Clamp & normalize, plus an additional inversion of the reference

(define (clamp&invert&normalize x lo hi) ; TODO: compose!
  (normalize (invert (clamp x lo hi) lo hi) lo hi))

;;; Cut the value if lower than

(define (cut-low x lo)
  (if (< x lo)
      lo
      x))

;;; Cut the value if higher than

(define (cut-high x hi)
  (if (> x hi)
      hi
      x))

;;; If greater than, otherwise return value

(define (if> a b f)
  (if (> a b)
      f
      a))

;;; If smaller than, otherwise return value

(define (if< a b f)
  (if (< a b)
      f
      a))


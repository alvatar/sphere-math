;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vector (dimension 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; vect2 type

(define-structure vect2 x y)

;;; Vector addition

(define (vect2:+vect2 v1 v2)
  (make-vect2 (+ (vect2-x v1)
                 (vect2-x v2))
              (+ (vect2-y v1)
                 (vect2-y v2))))

(define vect2+ vect2:+vect2)
;; (define-syntax vect2+
;;   (syntax-rules ()
;;     ((_ v)
;;      v)
;;     ((_ v1 v2)
;;      (vect2:+vect2 v1 v2))
;;     ((_ v1 v2 v3 ...)
;;      (vect2+ (vect2:+vect2 v1 v2) v3 ...))))

;;; Vector substraction

(define (vect2:-vect2 v1 v2)
  (make-vect2 (- (vect2-x v1)
                 (vect2-x v2))
              (- (vect2-y v1)
                 (vect2-y v2))))

(define vect2- vect2:-vect2)

;;; Vector dot product

(define (vect2:*vect2 v1 v2)
  (make-vect2 (* (vect2-x v1)
                 (vect2-x v2))
              (* (vect2-y v1)
                 (vect2-y v2))))

(define vect2* vect2:*vect2)

;;; Vector cross product

(define (vect2x v1 v2)
  (error "Not implemented"))

;;; Vector * scalar

(define (vect2:*scalar v a)
  (make-vect2 (* (vect2-x v) a)
              (* (vect2-y v) a)))

;;; Vector / scalar

(define (vect2:/scalar v a)
  (make-vect2 (/ (vect2-x v) a)
              (/ (vect2-y v) a)))

;;; Are these vectors equal? (with epsilon)

(define (vect2:= v1 v2)
  (and (= (vect2-x v1)
          (vect2-x v2))
       (= (vect2-y v1)
          (vect2-y v2))))

;;; Are these vectors proportional?

(define (vect2:proportional? v1 v2)
  (let ((v1x (vect2-x v1)) (v1y (vect2-y v1))
        (v2x (vect2-x v2)) (v2y (vect2-y v2)))
   (cond ((zero? v2x) (zero? v1x))
         ((zero? v2y) (zero? v1y))
         (else (= (/ v1x v2x)
                  (/ v1y v2y))))))

;;; Exact conversion

(define (vect2:inexact->exact v)
  (make-vect2 (inexact->exact (vect2-x v))
              (inexact->exact (vect2-y v))))

;;; Zero vector

(define (vect2:zero)
  (make-vect2 0 0))

;;; Random exact vector (range -1 -> 1)

(define (vect2:random)
  (make-vect2 (+ -1 (* (inexact->exact (random-real)) 2))
              (+ -1 (* (inexact->exact (random-real)) 2))))

;;; Calculate squared vector length

(define (vect2:squaredmagnitude vec)
  (+ (square (vect2-x vec))
     (square (vect2-y vec))))

;;; Calculate the symmetric vector

(define (vect2:symmetric vec)
  (make-vect2 (- (vect2-x vec))
              (- (vect2-y vec))))

;;; Calculate x projection

(define (vect2:x-projection vec)
  (make-vect2 (vect2-x vec)
              0))

;;; Calculate y projection

(define (vect2:y-projection vec)
  (make-vect2 0
              (vect2-y vec)))

;;; Get the component that is max

(define (vect2:max-component vec)
  (max (vect2-x vec)
       (vect2-y vec)))

;;; Get the component that is min

(define (vect2:min-component vec)
  (min (vect2-x vec)
       (vect2-y vec)))

;;; Calculate x/y ratio

(define (vect2:x/y vec)
  (/ (vect2-x vec)
     (vect2-y vec)))

;;; Calculate y/x ratio

(define (vect2:y/x vec)
  (/ (vect2-y vec)
     (vect2-x vec)))

;;; Absolute vector

(define (vect2:abs vec)
  (make-vect2 (abs (vect2-x vec))
              (abs (vect2-y vec))))

;;; Utility procedure to make a vector with each component 1.0 divided by the
;;; component of the given one

(define (vect2:1/vect2 vec)
  (make-vect2 (/ 1 (vect2-x vec))
              (/ 1 (vect2-y vec))))

;;; Clamp to low and high vect2

(define (vect2:clamp-vect2 vec lo-vec hi-vec)
  (let ((x (vect2-x vec))
        (y (vect2-y vec))
        (lox (vect2-x lo-vec))
        (loy (vect2-y lo-vec))
        (hix (vect2-x hi-vec))
        (hiy (vect2-y hi-vec)))
    (make-vect2
      (cond ((< x lox) lox)
            ((> x hix)  hix)
            (else x))
      (cond ((< y loy) lox)
            ((> y hiy) hiy)
            (else y)))))

;;; Clamp to low and high values

(define (vect2:clamp-values vec lo hi)
  (let ((x (vect2-x vec))
        (y (vect2-y vec)))
    (make-vect2
      (cond ((< x lo) lo)
            ((> x hi) hi)
            (else x))
      (cond ((< y lo) lo)
            ((> y hi) hi)
            (else y)))))


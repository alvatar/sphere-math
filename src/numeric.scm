;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numerical methods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Root finding
;-------------------------------------------------------------------------------

;;; Secant method for finding a root of a function

(define (root/secant f x0 x1)
  (error "Not implemented"))

;;; Find root via bracketing

(define (root/bracketing f x0 x1)
  (error "Not implemented"))

;;; Find root via Newton's method

(define (root/newton f x0 x1)
  (error "Not implemented"))

;;; Find root via quadratic interpolation method

(define (root/quadratic-interpolation)
  (error "Not implemented"))

;;; Find root via inverse quadratic interpolation method

(define (root/inverse-quadratic-interpolation)
  (error "Not implemented"))

;;; Find root via Brent's method

(define (root/brents f x0 x1)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Integration
;-------------------------------------------------------------------------------

;;;

(define (integrate/legendre-gauss ?)
  (error "Not implemented"))

;;;

(define (integrate/simpson ?)
  (error "Not implemented"))

;;;

(define (integrate/rectangle ?)
  (error "Not implemented"))

;;; 

(define (integrate/adaptive ?)
  (error "Not implemented"))

;;;

(define (integrate/trapezium ?)
  (error "Not implemented"))

;;;

(define (integrate/montecarlo ?)
  (error "Not implemented"))

;-------------------------------------------------------------------------------
; Misc
;-------------------------------------------------------------------------------

;;; Cosine similarity of input ranges a and b. The two ranges must have the same length.

(define (cosine-similarity l1 l2)
  (error "Not implemented"))

;;; Normalizes values in range by multiplying each element with a number chosen such that values sum up to sum. If elements in range sum to zero, assigns sum / range-length to all. Normalization makes sense only if all elements in range are positive. 

(define (normalize-values l)
  (error "Not implemented"))

;;; Computes entropy of input range r in bits. This function assumes (without checking) that the values in r are all in [0, 1]. For the entropy to be meaningful, often r should be normalized too (i.e., its values should sum to 1). The two-parameter version stops evaluating as soon as the intermediate result is greater than or equal to max.

(define (entropy l #!optional max-val)
  (error "Not implemented"))

;;; Computes the Kullback-Leibler divergence between input ranges a and b, which is the sum ai * log(ai / bi). The base of logarithm is 2. The ranges are assumed to contain elements in [0, 1]. Usually the ranges are normalized probability distributions, but this is not required or checked by kullbackLeiblerDivergence. If any element bi is zero and the corresponding element ai nonzero, returns infinity. (Otherwise, if ai == 0 && bi == 0, the term ai * log(ai / bi) is considered zero.) If the inputs are normalized, the result is positive.

(define (kullback-leibler-divergence l1 l2)
  (error "Not implemented"))

;;; Computes the Jensen-Shannon divergence between a and b, which is the sum (ai * log(2 * ai / (ai + bi)) + bi * log(2 * bi / (ai + bi))) / 2. The base of logarithm is 2. The ranges are assumed to contain elements in [0, 1]. Usually the ranges are normalized probability distributions, but this is not required or checked by jensenShannonDivergence. If the inputs are normalized, the result is bounded within [0, 1]. The three-parameter version stops evaluations as soon as the intermediate result is greater than or equal to limit.

(define (jensen-shannon-divergence l1 l2 #!optional limit)
  (error "Not implemented"))

;;; The so-called "all-lengths gap-weighted string kernel" computes a similarity measure between s and t based on all of their common subsequences of all lengths. Gapped subsequences are also included.
;;; TODO, see http://www.d-programming-language.org/phobos/std_numeric.html for greater explanation

(define (gap-weighted-similarity s t l)
  (error "Not implemented"))

;;; TODO, see http://www.d-programming-language.org/phobos/std_numeric.html for greater explanation

(define (gap-weighted-similarity-normalized s t l)
  (error "Not implemented"))

;;; TODO, see http://www.d-programming-language.org/phobos/std_numeric.html for greater explanation
;;; see struct GapWeightedSimilarityIncremental

(define (running-gap-weighted-similarity! s t l)
  (error "Not implemented"))

;;; Fast Fourier transform

(define (fft ?)
  (error "Not implemented"))

;;; Inverse fast Fourier transform

(define (ifft ?)
  (error "Not implemented"))

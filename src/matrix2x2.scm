;;; Copyright (c) 2012 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matrices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Calculate determinant of a 2x2 matrix

(define (determinant-2x2 a1 a2
                         b1 b2)
  (- (* a1 b2)
     (* b1 a2)))

;;; Calculate the row echelon form

(define (row-echelon-form m)
  (error "Not implemented"))


;;! Evaluate a polynomial using Horner's rule, given a list of the coefficients
(define (eval-polynomial/horner coeffs x)
  (error "Not implemented"))

;;! Find roots of a quadratic equation
;; Returns empty list if no solutions, otherwise list of solutions
(define (solve-quadratic a b c)
  (let ((d (- (* b b) (* 4 a c))))
    (cond ((< d 0) (list))
          ((= d 0) (list (/ (+ (- b) (sqrt d)) (* 2 a))))
          (else (list (/ (- (- b) (sqrt d)) (* 2 a))
                      (/ (+ (- b) (sqrt d)) (* 2 a)))))))

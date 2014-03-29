;;; Copyright (c) 2013-2014 by Álvaro Castro Castilla
;;; Vector-based matrices

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))


;;! make-matrix creates a matrix (a vector of vectors).
(define (make-matrix rows columns)
  (do ((m (make-vector rows))
       (i 0 (+ i 1)))
      ((= i rows) m)
    (vector-set! m i (make-vector columns)))) 

;;! matrix? checks to see if its argument is a matrix.
;;! It isn't foolproof, but it's generally good enough.
(define (matrix? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (vector? (vector-ref x 0)))) 

;;! matrix-rows returns the number of rows in a matrix.
(define (matrix-rows x)
  (vector-length x)) 

;;! matrix-columns returns the number of columns in a matrix.
(define (matrix-columns x)
  (vector-length (vector-ref x 0))) 

;;! matrix-ref returns the jth element of the ith row.
(define (matrix-ref m i j)
  (vector-ref (vector-ref m i) j)) 

;;! matrix-set! changes the jth element of the ith row.
(define (matrix-set! m i j x)
  (vector-set! (vector-ref m i) j x)) 

;;! map for matrices
(define (matrix:map f m)
  (let* ((nr (matrix-rows m))
         (nc (matrix-columns m))
         (r (make-matrix nr nc)))
    (do ((i 0 (+ i 1)))
        ((= i nr) r)
      (do ((j 0 (+ j 1)))
          ((= j nc))
        (matrix-set! r i j
                     (f (matrix-ref m i j)))))))

;;! * is the generic matrix/scalar multiplication procedure
(define (matrix:* x y)
  (letrec
      ((mat-sca-mul
        (lambda (m x)
          (let* ((nr (matrix-rows m))
                 (nc (matrix-columns m))
                 (r (make-matrix nr nc)))
            (do ((i 0 (+ i 1)))
                ((= i nr) r)
              (do ((j 0 (+ j 1)))
                  ((= j nc))
                (matrix-set! r i j
                             (* x (matrix-ref m i j))))))))
       (mat-mat-mul
        (lambda (m1 m2)
          (let* ((nr1 (matrix-rows m1))
                 (nr2 (matrix-rows m2))
                 (nc2 (matrix-columns m2))
                 (r   (make-matrix nr1 nc2)))
            (if (not (= (matrix-columns m1) nr2))
                (match-error m1 m2))
            (do ((i 0 (+ i 1)))
                ((= i nr1) r)
              (do ((j 0 (+ j 1)))
                  ((= j nc2))
                (do ((k 0 (+ k 1))
                     (a 0
                        (+ a
                           (* (matrix-ref m1 i k)
                              (matrix-ref m2 k j)))))
                    ((= k nr2)
                     (matrix-set! r i j a))))))))
       ;; invalid type of argument
       (type-error
        (lambda (what)
          (error "matrix:* -- argument is not a number or matrix")))
       ;; incompatible arguments
       (match-error
        (lambda (what1 what2)
          (error 'mul
                 "~s and ~s are incompatible operands"
                 what1
                 what2))))
    (cond
     ((number? x)
      (cond
       ((number? y) (* x y))
       ((matrix? y) (mat-sca-mul y x))
       (else (type-error y))))
     ((matrix? x)
      (cond
       ((number? y) (mat-sca-mul x y))
       ((matrix? y) (mat-mat-mul x y))
       (else (type-error y))))
     (else (type-error x)))))

(define (make-identity-matrix)
  '#(#(1 0 0 0)
     #(0 1 0 0)
     #(0 0 1 0)
     #(0 0 0 1)))

(define (make-translation-matrix x y z)
  `#(#(1 0 0 ,x)
     #(0 1 0 ,y)
     #(0 0 1 ,z)
     #(0 0 0 1)))

(define (make-scaling-matrix x y z)
  `#(#(,x 0 0 0)
     #(0 ,y 0 0)
     #(0 0 ,z 0)
     #(0 0 0 1)))

(define (make-x-rotation-matrix omega)
  (let ((cosO (cos omega))
        (sinO (sin omega)))
    (let ((-sinO (- sinO)))
      `#(#(1 0     0      0)
         #(0 ,cosO ,-sinO 0)
         #(0 ,sinO ,cosO  0)
         #(0 0     0      1)))))

(define (make-y-rotation-matrix omega)
  (let ((cosO (cos omega))
        (sinO (sin omega)))
    (let ((-sinO (- sinO)))
      `#(#(,cosO  0 ,sinO 0)
         #(0      1 0     0)
         #(,-sinO 0 ,cosO 0)
         #(0      0 0     1)))))

(define (make-z-rotation-matrix omega)
  (let ((cosO (cos omega))
        (sinO (sin omega)))
    (let ((-sinO (- sinO)))
      `#(#(,cosO ,-sinO 0 0)
         #(,sinO ,cosO  0 0)
         #(0     0      1 0)
         #(0     0      0 1)))))

(define (make-rotation-matrix x y z omega)
  (let ((C (cos omega))
        (S (sin omega)))
    (let ((iC (- 1 C))
          (iS (- 1 S)))
      `#(#(,(let ((x2 (* x x))) (+ x2
                                   (* (- 1 x2)
                                      C)))
           ,(- (* iC x y)
               (* z S))
           ,(+ (* iC x z)
               (* y S))
           0)
         #(,(+ (* iC x y)
               (* z S))
           ,(let ((y2 (* y y))) (+ y2
                                   (* (- 1 y2)
                                      C)))
           ,(- (* iC y z)
               (* x S))
           0)
         #(,(- (* iC x z)
               (* y S))
           ,(+ (* iC y z)
               (* x S))
           ,(let ((z2 (* z z))) (+ z2
                                   (* (- 1 z2)
                                      C)))
           0)
         #(0
           0
           0
           1)))))

(define (matrix:transpose m)
  (let* ((rows (matrix-rows m))
         (cols (matrix-columns m))
         (m2 (make-matrix cols rows)))
    (let loop ((r 0))
      (let loop2 ((c 0))
        (matrix-set! m2 r c (matrix-ref m c r))
        (if (< c (- cols 1)) (loop2 (+ c 1))))
      (if (< r (- rows 1)) (loop (+ r 1))))
    m2))


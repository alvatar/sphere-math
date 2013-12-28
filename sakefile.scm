(define modules
  '(basic
    bit-logical
    matrix))

(define-task clean ()
  (sake#default-clean))

(define-task compile ()
  (for-each (lambda (m)
              (sake#compile-module m)
              (sake#compile-module m compiler-options: '(debug)))
            modules))

(define-task install ()
  (for-each (lambda (m) (sake#install-compiled-module m versions: '(() (debug)))) modules))

(define-task all (compile install)
  'all)

(define modules
  '(basic
    bit-logical
    matrix))

(define-task clean ()
  (sake:default-clean))

(define-task compile ()
  (for-each (lambda (m)
              (sake:compile-c-to-o (sake:compile-to-c m))
              (sake:compile-c-to-o (sake:compile-to-c
                                    m
                                    version: '(debug)
                                    compiler-options: '(debug))))
            modules))

(define-task install ()
  (for-each (lambda (m) (sake:install-compiled-module m versions: '(() (debug)))) modules)
  (sake:install-sphere-to-system))

(define-task uninstall ()
  (sake:uninstall-sphere-from-system))

(define-task all (compile install)
  'all)

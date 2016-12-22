;; #!c:/Gauche/bin/gosh.exe

(define blank-regexp #/(^[ 　\t]+?$)/)
(define header-regexp #/^[ 　\t]+?/)

(define (folding-read-line)
  (port->string-list (standard-input-port)))

(define (line-cond line)
  (cond
   ((or (blank-regexp line)
	(equal? "" line)) "\n")
   (#t
    (regexp-replace-all header-regexp line ""))))

(define (main args)
  (print
   (fold (lambda (x y) (string-append y (line-cond x)))
   	 ""
   	 (folding-read-line))))

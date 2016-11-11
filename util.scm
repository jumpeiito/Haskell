(define ++ string-append)
(define +++ string-join)

(define-macro (aif cnd . clause)
  `(let ((it ,cnd))
     (if it ,@clause)))

(define (split-string-at str index)
  (values (string-take str (+ 1 index))
	  (string-drop str (+ 1 index))))

(define (date-day-week date)
  (case (date-week-day date)
    ((0) "日") ((1) "月") ((2) "火")
    ((3) "水") ((4) "木") ((5) "金")
    ((6) "土")))

(define (make-date-literally year month day)
  (make-date 0 0 0 0 day month year 9))

(define (group n l)
  (let loop ((r '()) (ls l))
    (cond
     ((> n (length ls))
      (if (null? ls)
	  (reverse r)
	  (reverse (cons ls r))))
     (#t
      (loop (cons (take ls n) r) (drop ls n))))))

(define (sum l) (fold + 0 l))

(define (scan reg str)
  (let loop ((s str) (r '()))
    (aif (reg s)
	 (loop (rxmatch-after it) (cons (rxmatch-substring it) r))
	 (reverse r))))

(define (regexp-replace-multiple-all regexp-alist str)
  (fold (lambda (x y)
	  (regexp-replace-all (car x) y (cdr x)))
	str
	regexp-alist))

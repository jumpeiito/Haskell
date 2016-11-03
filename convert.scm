(use srfi-13)
(use srfi-19)

(define translate-table
  '(
    ("mukai"	 . "向井")
    ("osima"	 . "大島")
    ("hitonaga"	 . "人長")
    ("hana"	 . "端")
    ("mine"	 . "峰")
    ("arai"	 . "荒井")
    ("tokui"	 . "得居")
    ("ito"	 . "伊東")
    ("yamaguchi" . "山口")
    ("kk"	 . "関電前行動")
    ("ks"	 . "関電スタンディングアピール")
    ("kokuho"	 . "建築国保")
    ("hb"        . "本部")
    ("khb"       . "京建労本部")
    ("is"        . "石田")
    ("h"         . "日野")
    ("o"         . "小栗栖")
    ("ic"        . "一言寺")
    ("s"         . "三宝院")
    ("t"         . "点在")
    ("d"         . "醍醐")
    ("f"         . "伏見")))

(define ++ string-append)
(define replace-regexp #/^,[a-z]+/)
(define time-regexp    #/^<[0-9]{4}>/)

(define-macro (aif cnd . clause)
  `(let ((it ,cnd))
     (if it ,@clause)))

(define (line-convert line)
  (cond
   ((#/^\* / line)       (++ "@@" (string-drop line 2)))
   ((#/^\*\* / line)     (++ "※" (string-drop line 3)))
   ((#/^\*\*\* / line)   (++ "　＊" (string-drop line 4)))
   ((#/^\*\*\*\* / line) (++ "　　～" (string-drop line 5)))
   (#t                 line)))

(define (%replace-convert key)
  (cdr (assoc (string-drop key 1)
	      translate-table)))

(define (replace-convert line)
  (let loop ((r "") (l line))
    (cond
     ((string= l "") r)
     ((replace-regexp l)
      (loop (++ r (%replace-convert (take-head-key l)))
	    (drop-head-key l)))
     (#t
      (loop (++ r (string-take l 1))
	    (string-drop l 1))))))

(define (drop-head-key line)
  (string-drop line
	       (+ 1 (aif (string-index line #\space)
			 it
			 (- (string-length line) 1)))))

(define (take-head-key line)
  (string-take line
	       (aif (string-index line #\space)
		    it
		    (string-length line))))

(define (%time-convert key)
  (let ((year (date-year (current-date)))
	(num4 (string->number
	       (string-drop (string-drop-right key 1) 1))))
    (receive (month day) (quotient&remainder num4 100)
	     (let ((dw (date-week-day
			(make-date 0 0 0 0 day month year 9))))
	       (format "~d/~d(~A)" month day
		       (case dw
			 ((0) "日")
			 ((1) "月")
			 ((2) "火")
			 ((3) "水")
			 ((4) "木")
			 ((5) "金")
			 ((6) "土")))))))

(define (time-convert line)
  (let loop ((r "") (l line))
    (cond
     ((string= l "") r)
     ((time-regexp l)
      (loop (++ r (%time-convert (string-take l 6)))
	    (string-drop l 6)))
     (#t
      (loop (++ r (string-take l 1))
	    (string-drop l 1))))))

(define main-translator (compose time-convert
				 replace-convert
				 line-convert))

(define (main args)
  (map
   (compose print main-translator)
   (string-split (port->string (standard-input-port))
		 "\n")))

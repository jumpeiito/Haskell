(use srfi-13)
(use srfi-19)
(use util.match)
(if (file-exists? "d:/home/Haskell/")
    (load "d:/home/Haskell/util.scm")
    (load "c:/Users/Jumpei/Haskell/util.scm"))

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
    ("zk"        . "交通費")
    ("zd"        . "行動費")
    ("kokuho"	 . "建築国保")
    ("hb"        . "本部")
    ("khb"       . "京建労本部")
    ("is"        . "石田")
    ("h"         . "日野")
    ("o"         . "小栗栖")
    ("ic"        . "一言寺")
    ("s"         . "三宝院")
    ("t"         . "点在")
    ("d"         . "醍醐支部")
    ("f"         . "伏見支部")
    ("shimbun"   . "しんぶん赤旗(日刊紙3,497円、日曜版823円)、京都民報623円")
    ("chik"      . "地区労分担金＠130×250円")))

(define replace-regexp #/^,[a-z]+/)
(define time-regexp    #/^<[,~0-9]+>/)
(define chapter-counter 0)
(define (both-strip str)
  (string-drop-right (string-drop str 1) 1))

(define (line-convert line)
  (cond
   ((#/^\* / line)       (++ (begin
			       (set! chapter-counter (+ 1 chapter-counter))
			       (format "~A　" chapter-counter))
			     (string-drop line 2)))
   ((#/^\*\* / line)     (++ "※" (string-drop line 3)))
   ((#/^\*\*\* / line)   (++ "　＊" (string-drop line 4)))
   ((#/^\*\*\*\* / line) (++ "　　～" (string-drop line 5)))
   (#t                 line)))

(define (chop str)
  (if (#/ $/ str)
      (string-drop-right str 1)
      str))

(define (%replace-convert key)
  (cdr (assoc (chop (string-drop key 1))
	      translate-table)))

(define (take-keywords line end-token)
  (split-string-at line
		   (aif (string-index line end-token)
			it
			(- (string-length line) 1))))

(define (collect-keywords line)
  (let loop ((r '()) (l line))
    (cond
     ((#/[0-9]{4}/ l)
      (loop (cons (string-take l 4) r)
	    (string-drop l 4)))
     ((#/[,~][0-9]{2}/ l)
      (loop (cons (%time-convert-lambda (string-take l 3)) r)
	    (string-drop l 3)))
     (#t (reverse r)))))

(define (%time-convert-lambda key)
  (lambda (year month)
    (let ((header (string-take key 1)))
      (++ (cond
	   ((string= header "~") "〜")
	   ((string= header ",") "、")
	   (else ""))
	  (%%time-convert year month
			  (string->number (string-drop key 1)))))))

(define (%time-convert-fold l)
  (define (detect-month key)
    (string->number (string-take key 2)))
  (define (detect-year month)
    (+ (if (> (date-month (current-date)) month)
	   1 0)
       (date-year (current-date))))
  (define (%time-convert4 key year)
    (receive (month day)
	(quotient&remainder (string->number key) 100)
      (format "~d/~d(~A)" month day
	      (date-day-week (make-date-literally year month day)))))
  (let ((year 0)(month 0))
    (fold (lambda (x y)
	    (if (string? x)
		(begin
		  (set! month (detect-month x))
		  (set! year  (detect-year month))
		  (++ y (%time-convert4 x year)))
		(++ y (x year month))))
	  ""
	  l)))

(define (%%time-convert year month day . arg)
  (let ((dw (date-day-week
	     (make-date-literally year month day))))
    (if (null? arg)
	(format "~d(~A)" day dw)
	(format "~d/~d(~A)" month day dw))))

(define (%time-convert key)
  (%time-convert-fold (collect-keywords (both-strip key))))

(define (convert-format line klass)
  (let ((regexp    (slot-ref klass 'regexp))
	(end-token (slot-ref klass 'end-token))
	(converter (slot-ref klass 'function)))
    (let loop ((r "") (l line))
      (cond
       ((string= l "") r)
       ((regexp l)
	(receive (key rest) (take-keywords l end-token)
	  (loop (++ r (converter key)) rest)))
       (#t
	(loop (++ r (string-take l 1))
	      (string-drop l 1)))))))

(define (colnum str)
  (let ((slist (reverse (group 3 (reverse (string->list str))))))
    (string-reverse
     (fold (lambda (x y)
	     (cond
	      ((string= y "") (list->string x))
	      ((null? x) y)
	      (#t (string-join `(,(list->string x)
				 ,y) ","))))
	   ""
	   slist))))

(define (colnum2 str)
  (colnum (string-drop-right (string-drop str 1) 1)))

(define (colnum-yen str)
  (++ (colnum (string-drop-right (string-drop str 1) 1))
      "円"))

(define (colnum-yen-kodohi str)
  (++ "行動費"
      (colnum (string-drop-right (string-drop str 3) 1))
      "円"))

(define (colnum-yen-kotsuhi str)
  (++ "交通費"
      (colnum (string-drop-right (string-drop str 3) 1))
      "円"))

(define (colnum-by per num)
  (+++ `("@" ,(colnum per) "×" ,(colnum num) "人") ""))

(define (colnum-kumiaihi str)
  (match (string-split (both-strip str) ",")
    ((ip mi ro)
     (++ (colnum-by "1342" ip) "、"
	 (colnum-by "1332" mi) "、"
	 (colnum-by "882" ro)))
    (#t "")))

(define-class <Converter> ()
  ((regexp    :accessor regexp    :init-keyword :regexp)
   (end-token :accessor end-token :init-keyword :end-token)
   (function  :accessor function  :init-keyword :function)))

(define-macro (define-converter name . clause)
  `(begin
     (define ,name (make <Converter> ,@clause))
     (define ,(string->symbol
	       (string-drop-right (symbol->string name) 2))
       (lambda (line) (convert-format line ,name)))))

(define-converter replace-converter
  :regexp replace-regexp
  :end-token #\space
  :function %replace-convert)
(define-converter time-converter
  :regexp time-regexp
  :end-token #\>
  :function %time-convert)
(define-converter yen-converter
  :regexp #/^\[[0-9]+\]/
  :end-token #\]
  :function colnum-yen)
(define-converter kodohi-converter
  :regexp #/^\[zd[0-9]+\]/
  :end-token #\]
  :function colnum-yen-kodohi)
(define-converter kotsuhi-converter
  :regexp #/^\[zk[0-9]+\]/
  :end-token #\]
  :function colnum-yen-kotsuhi)
(define-converter kumiaihi-converter
  :regexp #/^{[0-9,]+}/
  :end-token #\}
  :function colnum-kumiaihi)

(define main-translator (compose time-convert
				 replace-convert
				 line-convert
				 yen-convert
				 kumiaihi-convert
				 kodohi-convert
				 kotsuhi-convert))

(define (yen-string->number str)
  (string->number
   (fold (lambda (x y) (regexp-replace-all x y ""))
	 str
	 (list #/,/ #/円/))))

(define (multiple-yen-string->number str)
  (aif (#/[@＠]([0-9,]+)円*×([0-9,]+)人*/ str)
       (* (yen-string->number (it 1))
	  (yen-string->number (it 2)))))

(define (yen-string-translator str)
  (if (#/^[@＠]/ str)
      (multiple-yen-string->number str)
      (yen-string->number str)))

(define (read-calc str)
  (fold + 0
	(map yen-string-translator
	     (scan yen-scan-regexp str))))

(define (main args)
  (if (and (equal? 2 (length args))
	   (equal? "-e" (cadr args)))
      (print (+++ `(" --> "
		    ,(colnum
		      (number->string
		       (read-calc (port->string (standard-input-port)))))
		    "円")
		  ""))
      (map
       (compose print main-translator)
       (string-split (port->string (standard-input-port))
		     "\n"))))

(define yen-scan-regexp #/([0-9,]+円)|[@＠][0-9,]+円*×[0-9,]+人*/)

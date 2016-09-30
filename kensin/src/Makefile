all: rec.pdf meibo.pdf

rec.pdf:rec.tex insert.tex
	f:/tex/bin/platex.exe -kanji=utf8 rec.tex
	f:/tex/bin/dvipdfmx.exe -p a4 rec.dvi
	rm *.log *.aux *.dvi

meibo.pdf: meibo.tex meiboInsert.tex
	f:/tex/bin/platex.exe -kanji=utf8 meibo.tex
	f:/tex/bin/dvipdfmx.exe -p a4 meibo.dvi
	rm *.log *.aux *.dvi

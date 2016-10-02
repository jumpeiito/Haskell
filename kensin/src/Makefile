TEX=f:/tex/bin/platex.exe
TEXOPT=-kanji=utf8
DVIPDF=f:/tex/bin/dvipdfmx.exe

all: rec.pdf meibo.pdf

rec.pdf:rec.tex insert.tex
	${TEX} ${TEXOPT} rec.tex
	${DVIPDF} -p a4 rec.dvi
	rm *.log *.aux *.dvi

meibo.pdf: meibo.tex meiboInsert.tex
	${TEX} ${TEXOPT} meibo.tex
	${DVIPDF} -p a4 meibo.dvi
	rm *.log *.aux *.dvi

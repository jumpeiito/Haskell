TEX=f:/tex/bin/platex.exe
TEXOPT=-kanji=utf8
DVIPDF=f:/tex/bin/dvipdfmx.exe
TOPDIR=s:/原田フォルダ/2016年度/健康診断/16秋の健康診断
MEIBO=${TOPDIR}/日曜健診名簿.pdf
REC=${TOPDIR}/領収書.pdf

all: rec.pdf meibo.pdf Makefile

rec.pdf:rec.tex insert.tex
	${TEX} ${TEXOPT} rec.tex
	${DVIPDF} -p a4 rec.dvi
	cp -f rec.pdf ${REC}
	rm *.log *.aux *.dvi

meibo.pdf: meibo.tex meiboInsert.tex
	${TEX} ${TEXOPT} meibo.tex
	${DVIPDF} -p a4 meibo.dvi
	cp -f meibo.pdf ${MEIBO}
	rm *.log *.aux *.dvi

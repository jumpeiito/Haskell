PLATEX="c:/tex/bin/platex.exe"
DVIPDFMX="c:/tex/bin/dvipdfmx.exe"

test.pdf:test.tex Makefile
	${PLATEX} -kanji=utf8 test.tex
	${DVIPDFMX} -p "120mm,235mm" test.dvi
	rm *.log *.aux *.dvi


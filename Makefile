hancho.pdf:hancho.tex hancho.insert.tex
	f:/tex/bin/platex.exe -kanji=utf8 hancho.tex
	f:/tex/bin/dvipdfmx.exe -p "24cm,37cm" hancho.dvi

snews.exe: snews.hs *.hs */*.hs
	ghc -O3 snews.hs

tags: *.hs
	./hasktags.exe -e *.hs */*.hs

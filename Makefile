hancho.pdf:hancho.tex hancho.insert.tex
	f:/tex/bin/platex.exe -kanji=utf8 hancho.tex
	f:/tex/bin/dvipdfmx.exe -p "24cm,37cm" hancho.dvi

snews.exe: snews.hs *.hs */*.hs
	ghc -O3 snews.hs

tags: *.hs
	./hasktags.exe -e *.hs */*.hs

zipcode.exe: zipcode.hs util.hs
	ghc -O3 zipcode.hs

# 実行するときは./zipcode.exe +RTS -p
zipcode-profile: zipcode.hs util.hs
	ghc -v -prof -fprof-auto -rtsopts zipcode.hs

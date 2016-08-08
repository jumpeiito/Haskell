ghc=`which ghc.exe`

all: meibo.exe snews.exe tags zipcode.exe

hancho.pdf:hancho.tex hancho.insert.tex
	f:/tex/bin/platex.exe -kanji=utf8 hancho.tex
	f:/tex/bin/dvipdfmx.exe -p "24cm,37cm" hancho.dvi

meibo.exe: meibo.hs util.hs strdt.hs telephone.hs
	${ghc} -O3 meibo.hs

snews.exe: snews.hs *.hs */*.hs
	${ghc} -O3 snews.hs

tags: *.hs
	./hasktags.exe -e *.hs */*.hs

zipcode.exe: zipcode.hs util.hs zipDist.hs
	${ghc} -O3 zipcode.hs -threaded

zipcode-profile: zipcode.hs util.hs zipDist.hs
	echo "実行するときは./zipcode.exe +RTS -p | nkfwin32.exe -Ws"
	${ghc} -threaded -prof -fprof-auto -rtsopts zipcode.hs


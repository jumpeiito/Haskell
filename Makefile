ghc=`which ghc.exe`
FIND=f:/UnxUtils/usr/local/wbin/find.exe

all: zipcode-profile hancho.pdf tags

hancho.pdf:hancho.tex hancho.insert.tex
	f:/tex/bin/platex.exe -kanji=utf8 hancho.tex
	f:/tex/bin/dvipdfmx.exe -p "24cm,37cm" hancho.dvi

tags: Makefile
	${FIND} f:\Haskell -name '*.hs' ! -name '*Paths*' | xargs hasktags.exe -e

zipcode-profile: zipcode.hs util.hs zipDist.hs
	echo "実行するときは./zipcode.exe +RTS -p | nkfwin32.exe -Ws"
	${ghc} -threaded -prof -fprof-auto -rtsopts zipcode.hs


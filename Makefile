ghc=`which ghc.exe`
FIND=f:/UnxUtils/usr/local/wbin/find.exe

all:
	cd f:/Haskell/Snews
	stack build
	cd f:/Haskell/Meibo
	stack build
	cd f:/Haskell/Util
	stack build
	cd f:/Haskell/Zipcode
	stack build

hancho.pdf:hancho.tex hancho.insert.tex
	f:/tex/bin/platex.exe -kanji=utf8 hancho.tex
	f:/tex/bin/dvipdfmx.exe -p "24cm,37cm" hancho.dvi

tags: Makefile
	${FIND} f:/Haskell -name '*.hs' ! -name '*Paths*' | xargs hasktags.exe -e


echo "Haskell Program build up"
stack build
echo "extracting receipt data"
stack exec -- kensin.exe -r > ./insert.tex
echo "extracting meibo data"
stack exec -- kensin.exe -m > ./meiboInsert.tex
echo "make pdf"
make all

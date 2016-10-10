echo "Haskell Program build up"
stack build
echo "extracting receipt data"
kensin.exe -r > ./insert.tex
echo "extracting meibo data"
kensin.exe -m > ./meiboInsert.tex
echo "make pdf"
make all

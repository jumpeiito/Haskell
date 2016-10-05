DIR=c:/Users/sibuc526.NEWNET/AppData/Roaming/local/bin/

echo "Haskell Program build up"
stack build
echo "extracting receipt data"
${DIR}/kensin.exe -r > ./insert.tex
echo "extracting meibo data"
${DIR}/kensin.exe -m > ./meiboInsert.tex
echo "make pdf"
make all

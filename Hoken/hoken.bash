yearMonth=`date +%Y%m`
Hoken-exe.exe --pdf $1 --date ${yearMonth}$2 --second > ./hagaki.insert.tex
Hoken-exe.exe --pdf $1 --date ${yearMonth}$2 --first > ./Atena.insert.tex
make all

cp -f ./Atena.pdf s:/�R���t�H���_/�ی���/���Њ֌W/�������x��${yearMonth}.pdf
cp -f ./hagaki.pdf s:/�R���t�H���_/�ی���/���Њ֌W/��${yearMonth}.pdf

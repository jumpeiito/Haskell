yearMonth=`date +%Y%m`
Hoken-exe.exe --pdf $1 --date ${yearMonth}$2 --second > ./hagaki.insert.tex
Hoken-exe.exe --pdf $1 --date ${yearMonth}$2 --first > ./Atena.insert.tex
make all

cp -f ./Atena.pdf s:/山口フォルダ/保険料/除籍関係/宛名ラベル${yearMonth}.pdf
cp -f ./hagaki.pdf s:/山口フォルダ/保険料/除籍関係/督促${yearMonth}.pdf

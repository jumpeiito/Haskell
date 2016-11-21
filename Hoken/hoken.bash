yearMonth=`date +%Y%m`
Hoken-exe.exe --pdf $1 --date ${yearMonth}$2 --second > ./hagaki.insert.tex
Hoken-exe.exe --pdf $1 --date ${yearMonth}$2 --first > ./Atena.insert.tex
make all

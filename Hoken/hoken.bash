yearMonth=`date +%Y%m`
Hoken-exe.exe $1 ${yearMonth}$2 > ./hagaki.insert.tex
make

MEIBO=`which meibo.exe`

cd d:/home/Haskell/Meibo/
echo "extract -----> tex file"
${MEIBO} hchp $1 $2 $3 _ _ > ./hancho.insert.tex
echo "compiling ---> pdf file"
make hancho.pdf
echo "viewing -----> Acrobat Reader"
"c:/Program Files/Adobe/Acrobat Reader DC/Reader/AcroRd32.exe" ./hancho.pdf
cd -

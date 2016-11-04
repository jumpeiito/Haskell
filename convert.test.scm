(use gauche.test)

(load "./convert.scm")

(test-start "convert-test")

(test-section "replace-convert test")
(test* "conversion1"
       "向井大島人長端峰荒井得居伊東山口"
       (replace-convert ",mukai ,osima ,hitonaga ,hana ,mine ,arai ,tokui ,ito ,yamaguchi"))
(test* "conversion2"
       "関電前行動関電スタンディングアピール交通費行動費建築国保本部京建労本部石田日野小栗栖一言寺三宝院点在"
       (replace-convert ",kk ,ks ,zk ,zd ,kokuho ,hb ,khb ,is ,h ,o ,ic ,s ,t"))
(test* "conversion3"
       "醍醐支部伏見支部しんぶん赤旗(日刊紙3,497円、日曜版823円)、京都民報623円地区労分担金＠130×250円"
       (replace-convert ",d ,f ,shimbun ,chik"))

(test-section "line-convert test")
(test* "conversion1" "1　hoge" (line-convert "* hoge"))
(test* "conversion2" "2　hoge" (line-convert "* hoge"))
(test* "conversion3" "3　hoge" (line-convert "* hoge"))
(test* "conversion4" "4　hoge" (line-convert "* hoge"))
(test* "conversion5" "※hoge" (line-convert "** hoge"))
(test* "conversion6" "　＊hoge" (line-convert "*** hoge"))
(test* "conversion7" "　　～hoge" (line-convert "**** hoge"))

(test-section "time-convert test(2016)")
(test* "conversion1" "11/4(金)" (time-convert "<1104>"))
(test* "conversion2" "11/4(金)、5(土)" (time-convert "<1104,05>"))
(test* "conversion3" "11/4(金)、5(土)〜12(土)" (time-convert "<1104,05~12>"))
(test* "conversion4" "11/4(金)、5(土)〜12(土)、18(金)" (time-convert "<1104,05~12,18>"))

(test-section "yen-convert test")
(test* "conversion1" "1円" (yen-convert "[1]"))
(test* "conversion2" "10円" (yen-convert "[10]"))
(test* "conversion3" "100円" (yen-convert "[100]"))
(test* "conversion4" "1,000円" (yen-convert "[1000]"))
(test* "conversion5" "10,000円" (yen-convert "[10000]"))
(test* "conversion6" "100,000円" (yen-convert "[100000]"))
(test* "conversion7" "1,000,000円" (yen-convert "[1000000]"))
(test* "conversion8" "10,000,000円" (yen-convert "[10000000]"))
(test* "conversion9" "100,000,000円" (yen-convert "[100000000]"))
(test* "conversion10" "1,000,000,000円" (yen-convert "[1000000000]"))


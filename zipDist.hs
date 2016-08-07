module ZipDist where

data District = District String [String] deriving (Show)

fromDistrict :: District -> [String]
fromDistrict (District _ x) = x

kyotoDistrict, fushimiDistrict, shigaDistrict, ujiDistrict, yamashinaDistrict :: District
kyotoDistrict     = District "京都市"      ["北区", "上京区", "中京区", "下京区", "左京区", "右京区", "西京区", "南区", "伏見区", "東山区"]
fushimiDistrict   = District "京都市伏見区" ["醍醐", "石田", "日野", "小栗栖", "桃山", "深草", "向島", "羽束師", "久我",
                                            "横大路", "納所", "竹田", "下鳥羽", "中島", "淀"]
shigaDistrict     = District "滋賀県"      ["大津市", "草津市", "栗東市"]
ujiDistrict       = District "京都府"      ["宇治市", "城陽市", "八幡市", "向日市", "長岡京市", "京田辺市"]
yamashinaDistrict = District "京都市"      ["山科区"]


import Data.List
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified System.IO                  as I
import qualified System.Directory           as SD
import Codec.Text.IConv (convert)
import Codec.Binary.UTF8.String (encodeString)
import Control.Applicative

(</>) :: String -> FilePath -> FilePath
(</>) dir fp =
  if isSuffixOf dir "/"
  then dir ++ fp
  else dir ++ "/" ++ fp

recFileList :: FilePath -> IO [String]
recFileList fp = do
  list <- map (fp </>) <$> SD.getDirectoryContents fp
  
  return $ foldl (\l x -> bool <- SD.doesDirectoryExist x; if bool then (l ++ recFileList x) else x:l) [] list
  
  

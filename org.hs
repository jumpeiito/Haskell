import Util
import OrgQuery
import Data.Time
import Data.Time.Format
import Data.Time.Calendar
import Text.Printf
import Text.Regex.Posix
import Codec.Binary.UTF8.String
import System.Environment (getArgs)
import System.IO (IOMode (..), hGetContents, hSetEncoding, openFile, hClose, mkTextEncoding, stdout, utf8)

initOrg :: Day -> Org
initOrg d =
  Org { number   = 0,
        date     = d,
        paper    = "",
        priority = "",
        title    = "",
        tag      = Nop,
        list     = [] }

addLine :: Org -> Lines -> Org
addLine org l@(Line s) = org { list = l:list org }
addLine org _          = org

addRevLine :: Org -> Lines -> Org
addRevLine org l@(Line s) = org { list = reverse $ l:list org }
addRevLine org _          = org

regMatch :: String -> String -> Bool
regMatch str reg = str =~ reg :: Bool

abstractTags :: String -> Tag
abstractTags s =
  case func s of
    [] -> Nop
    s  -> T s
  -- where func = filter (\m -> not (m == "")) . tail . split ':'
  where func = filter (/="") . tail . split ':'

abstractPaper :: String -> String
abstractPaper s
  | s `regMatch` ".*\\[朝日.*" = "朝日"
  | s `regMatch` ".*\\[読売.*" = "読売"
  | s `regMatch` ".*\\[毎日.*" = "毎日"
  | s `regMatch` ".*\\[日経.*" = "日経"
  | s `regMatch` ".*\\[産経.*" = "産経"
  | s `regMatch` ".*\\[東京.*" = "東京"
  | otherwise = "赤旗"

abstractPriority :: String -> String
abstractPriority s
  | s `regMatch` ".*\\[#[aA]\\].*" = "A"
  | s `regMatch` ".*\\[#[bB]\\].*" = "B"
  | s `regMatch` ".*\\[#[cC]\\].*" = "C"
  | otherwise = ""
                
headerStrip :: String -> String
headerStrip "" = ""
headerStrip y@(x:xs)
  | x `elem` ignoreList = headerStrip xs
  | otherwise           = y
  where ignoreList = " \t*"

headerStrip_ :: String -> String
headerStrip_ = reverse . headerStrip . reverse

serialize :: String -> Lines
serialize "" = Blank
serialize s
  | s `regMatch` "^\\*\\* " =
    let title    = headerStrip_ $ (head . split ':') $ headerStrip s
        paper    = abstractPaper s
        priority = abstractPriority s
        tags     = abstractTags s in
    Header title paper priority tags
  | s `regMatch` "^\\* [0-9]+" = OrgDate $ string2Date $ headerStrip s
  | otherwise                  = Line $ headerStrip s
  
foldOrg :: [Lines] -> [Org]
foldOrg l' =
  loop l' org "" 1 []
  where org = initOrg $ string2Date "1900/1/1"
        loop [] o s n r = reverse (o:r)
        loop (x:xs) o s n r =
          case x of
          OrgDate d          -> loop xs (initOrg d) "" n r
          Line s'            -> loop xs o (s++s') n r
          Header tt p pr tag -> loop xs (o { title=tt, paper=p, priority=pr, tag=tag, number=n}) "" (1+n) (o':r)
            where o' = addRevLine o (Line s)
          Blank              -> loop xs o' "" n r
            where o' = addLine o (Line s)

puts :: Lines -> IO ()
puts l = 
  case l of
    Blank              -> putStrLn ""
    Line s             -> putStrLn s
    Header title _ _ _ -> putStrLn title
    OrgDate d          -> print d

orgLinePuts :: Org -> IO ()
orgLinePuts org = mapM_ puts $ list org

orgHeaderPuts :: Org -> IO ()
orgHeaderPuts org = do
  putStrLn $ "Org {title=" ++ title org ++ ","
  putStr   $ "paper=" ++ paper org
  putStr   $ ", date=" ++ show $ date org
  putStrLn $ ", tags=" ++ connect "," $ tagList org ++ "}"

orgHeaderSimplePuts :: Org -> IO ()
orgHeaderSimplePuts org = 
  putStrLn $ printf "%04d %s %s %s%s%s" n d p t arrow tg
  where n = number org
        d = show $ date org
        p = paper org
        t = title org
        tg = connect "," $ tagList org
        arrow = if null tg then "" else "-->"

orgPuts :: Org -> IO ()
orgPuts org = do
  orgHeaderPuts org
  orgLinePuts org
  
main :: IO ()
main = do
  [filepath, query] <- getArgs
  contents <- readUTF8File filepath
  let serializedData = map serialize $ lines contents 
  let foldedData = filter (askQuery query) $ foldOrg serializedData
  hSetEncoding stdout utf8
  mapM_ orgHeaderSimplePuts foldedData

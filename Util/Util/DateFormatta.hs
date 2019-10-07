module Util.DateFormatta where

import           Control.Arrow
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer.Strict
import qualified Data.Text.Lazy.Builder    as TLB
import qualified Data.Text.Lazy            as TL
import qualified Data.Text                 as T
import           Data.Time

type DateFormat a =
  ReaderT (Integer, Int, Int) (Writer TLB.Builder) a

runDF :: DateFormat a -> Day -> TL.Text
runDF df d =
  TLB.toLazyText $ execWriter $ df `runReaderT` (toGregorian d)

addA :: (Num a, Show a) => a -> DateFormat ()
addA = show >>> T.pack >>> TLB.fromText >>> tell >>> lift

year_ :: DateFormat ()
year_ = do
  (y, _, _) <- ask
  addA y

month_ :: DateFormat ()
month_ = do
  (_, m, _) <- ask
  addA m

{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Util.StrEnum ( forChar_
                    , forCharI_
                    , mapChar_
                    , mapCharI_
                    , split
                    , stringFold) where

import Control.Monad.State
import Text.StringLike                  (StringLike, castString)
import Data.Monoid                      ((<>))
import qualified Data.Text              as Tx
import qualified Data.Text.Internal     as Txi
import qualified Data.ByteString.Char8  as B

class (Monoid a, StringLike a) => StrEnum a where
  forChar_   :: (Monad m) => a -> (Char -> m b) -> m ()
  forCharI_  :: (Monad m) => a -> (Char -> Int -> m b) -> m ()
  mapChar_   :: (Monad m) => (Char -> m b) -> a -> m ()
  mapCharI_  :: (Monad m) => (Char -> Int -> m b) -> a -> m ()
  mapChar_  = flip forChar_
  mapCharI_ = flip forCharI_
  split      :: Char -> a -> [a]
  stringFold :: Int -> String -> a -> a
  stringFold col adder t = fst . (`execState` (mempty, 0))
                           $ _stringFold col adder t
--------------------------------------------------
instance StrEnum String where
  forChar_ = forM_
  forCharI_ str f = do
    let l = [(str!!i, i) | i <- [0..(length str - 1)]]
    forM_ l $ \(ch, num) -> f ch num
--------------------
  split sep str = reverse . fst . (`execState` (mempty, mempty)) $ do
    forM_ str $ \ch -> do
      (big, small) <- get
      if ch == sep
        then put (reverse small:big, [])
        else put (big, ch : small)
    (big, small) <- get
    put (reverse small:big, [])
--------------------------------------------------
instance StrEnum B.ByteString where
  forChar_  = forCharCombinator_ B.index B.length
  forCharI_ = forCharICombinator_ B.index B.length
--------------------
  split sep bstr = map B.pack $ split sep $ B.unpack bstr
--------------------------------------------------
instance StrEnum Txi.Text where
  forChar_  = forCharCombinator_ Tx.index Tx.length
  forCharI_ = forCharICombinator_ Tx.index Tx.length
--------------------
  split sep bstr = map Tx.pack $ split sep $ Tx.unpack bstr
----------------------------------------------------------------------------------------------------
_stringFold ::
  (StrEnum a, Monoid a, StringLike a) =>
  Int -> String -> a -> State (a, Int) ()
_stringFold column adder t = do
  forChar_ t $ \ch -> do
    (text, count) <- get
    let (plus, c) | count == column = (adder,  0)
                  | otherwise       = (mempty, count + 1)
    put (text <> castString (ch : plus), c)

forCharCombinator_ indexf lengthf str f =
  mapM_ f [ indexf str n | n <- [0..(lengthf str)-1]]
forCharICombinator_ indexf lengthf str f = do
  let l = [ (indexf str n, n) | n <- [0..(lengthf str)-1]]
  forM_ l $ \(ch, num) -> f ch num


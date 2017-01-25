{-# LANGUAGE DataKinds #-}

module Util.Kumiaihi where

import Data.Time

data Personal = Personal Day | Family Gender Day deriving Show

data Gender   = Male | Female deriving (Show, Eq)

--- Special1 21


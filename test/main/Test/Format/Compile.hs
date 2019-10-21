-- Tests succeed if module compiles
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Format.Compile
    (
    ) where

import Data.Time

newtype WrappedUTCTime =
    MkWrappedUTCTime UTCTime
    deriving (FormatTime, ParseTime)

newtype Wrapped t =
    MkWrapped t
    deriving (FormatTime, ParseTime)

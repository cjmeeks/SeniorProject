{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Api.Elm
    ( ElmUTCTime(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID, toText)
import Elm (ElmType(..))
import GHC.Generics (Generic)

newtype ElmUTCTime = ElmUTCTime
    { getUTCTime :: UTCTime
    } deriving (Show, ToJSON, FromJSON)

instance ElmType ElmUTCTime where
    toElmType = \(ElmUTCTime time) -> toElmType (show time)

instance ElmType UUID where
    toElmType = \uuid -> toElmType (toText uuid)
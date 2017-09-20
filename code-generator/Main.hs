{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where
import Data.Proxy (Proxy (Proxy))
import Elm (Spec (Spec), specsToDir, toElmTypeSource, toElmDecoderSource, toElmEncoderSource)
import Servant.Elm (ElmOptions (..), defElmImports, defElmOptions, generateElmForAPIWith, UrlPrefix (Static))
import Api.Types
import Data.Text as DT
import Api.App.Types (Dice(..))
import Api.Types (UserInfo(..), Workout(..), Exercise(..), Run(..), User(..))

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "/servant-elm-template" }

elmImports :: [Text]
elmImports = [ "import Dict exposing (Dict)"
              ]

specs :: [Spec]
specs =
  [ 
    Spec ["Shared", "Generated"]
         ( defElmImports `append` (DT.intercalate "\n" elmImports)
         : toElmTypeSource (Proxy :: Proxy Dice)
         : toElmDecoderSource (Proxy :: Proxy Dice)
         : toElmEncoderSource (Proxy :: Proxy Dice)
         : toElmTypeSource (Proxy :: Proxy UserInfo)
         : toElmEncoderSource (Proxy :: Proxy UserInfo)
         : toElmDecoderSource (Proxy :: Proxy UserInfo)
         : toElmTypeSource (Proxy :: Proxy Workout)
         : toElmEncoderSource (Proxy :: Proxy Workout)
         : toElmDecoderSource (Proxy :: Proxy Workout)
         : toElmTypeSource (Proxy :: Proxy Exercise)
         : toElmEncoderSource (Proxy :: Proxy Exercise)
         : toElmDecoderSource (Proxy :: Proxy Exercise)
         : toElmTypeSource (Proxy :: Proxy Run)
         : toElmEncoderSource (Proxy :: Proxy Run)
         : toElmDecoderSource (Proxy :: Proxy Run)
         : toElmTypeSource (Proxy :: Proxy User)
         : toElmEncoderSource (Proxy :: Proxy User)
         : toElmDecoderSource (Proxy :: Proxy User)
         : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api)
         )
  ]

main :: IO ()
main = specsToDir specs "frontend/elm"
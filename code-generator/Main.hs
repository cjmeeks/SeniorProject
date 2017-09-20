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
import Api.Types (User(..))

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
         : toElmTypeSource (Proxy :: Proxy User)
         : toElmEncoderSource (Proxy :: Proxy User)
         : toElmDecoderSource (Proxy :: Proxy User)
         : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api)
         )
  ]

main :: IO ()
main = specsToDir specs "frontend/elm"
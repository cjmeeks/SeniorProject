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
import Api.Types (Workout, Exercise, Run, User, Lift, Set)

elmOpts :: ElmOptions
elmOpts =
  defElmOptions
    { urlPrefix = Static "" }

elmImports :: [Text]
elmImports = [ "import Dict exposing (Dict)"
              ]

specs :: [Spec]
specs =
  [ 
    Spec ["Shared", "Generated"]
         ( defElmImports `append` (DT.intercalate "\n" elmImports)
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
         : toElmTypeSource (Proxy :: Proxy Lift)
         : toElmEncoderSource (Proxy :: Proxy Lift)
         : toElmDecoderSource (Proxy :: Proxy Lift)
         : toElmTypeSource (Proxy :: Proxy Set)
         : toElmEncoderSource (Proxy :: Proxy Set)
         : toElmDecoderSource (Proxy :: Proxy Set)
         : generateElmForAPIWith elmOpts  (Proxy :: Proxy Api)
         )
  ]

main :: IO ()
main = specsToDir specs "../client/elm"
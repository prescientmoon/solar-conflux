module Thumbor
  ( RequiredResizingConfig
  , RequiredThumborConfig
  , OptionalResizingConfig
  , OptionalThumborConfig
  , Thumbor
  , VAlign(..)
  , HAlign(..)
  , mkThumbor
  , resize
  , fitIn
  , setPath
  , setSecurityKey
  , buildUrl
  , smartCrop
  , metadataOnly
  , crop
  , vAlign
  , hAlign
  ) where

import Data.Function.Uncurried (Fn4, runFn4)
import Prelude ((<<<))
import Thumbor.Internals (WithConfig)

type RequiredThumborConfig r
  = ( serverUrl :: String
    | r
    )

type OptionalThumborConfig
  = ( securityKey :: String
    , imagePath :: String
    )

type RequiredResizingConfig r
  = ( width :: Number, height :: Number
    | r
    )

type OptionalResizingConfig
  = ( flipVertically :: Boolean, flipHorizontally :: Boolean
    )

data VAlign
  = Top
  | Middle
  | Bottom

--  I don't call those "Right" and "Left" to avoid name collisions with the commonly used Data.Either module.
data HAlign
  = RightAlign
  | Center
  | LeftAlign

foreign import data Thumbor :: Type

foreign import mkThumbor ::
  forall config rest. WithConfig config rest OptionalThumborConfig RequiredThumborConfig -> Thumbor

foreign import resize ::
  forall config rest.
  WithConfig config rest OptionalResizingConfig RequiredResizingConfig ->
  Thumbor ->
  Thumbor

foreign import fitIn :: Number -> Number -> Thumbor -> Thumbor

foreign import setPath :: String -> Thumbor -> Thumbor

foreign import setSecurityKey :: String -> Thumbor -> Thumbor

foreign import buildUrl :: Thumbor -> String

foreign import smartCrop :: Thumbor -> Thumbor

foreign import metadataOnly :: Thumbor -> Thumbor

foreign import cropImpl :: Fn4 Int Int Int Int (Thumbor -> Thumbor)

foreign import vAlignImpl :: String -> Thumbor -> Thumbor

foreign import hAlignImpl :: String -> Thumbor -> Thumbor

crop :: Int -> Int -> Int -> Int -> Thumbor -> Thumbor
crop = runFn4 cropImpl

vAlign :: VAlign -> Thumbor -> Thumbor
vAlign =
  vAlignImpl
    <<< case _ of
        Top -> "top"
        Middle -> "middle"
        Bottom -> "bottom"

hAlign :: HAlign -> Thumbor -> Thumbor
hAlign =
  hAlignImpl
    <<< case _ of
        LeftAlign -> "left"
        Center -> "center"
        RightAlign -> "right"

module Main where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Data.Array as Array
import Data.Identity (Identity(..))
import Effect (Effect)
import Effect.Console (log)
import Safe.Coerce (coerce)
import Swictheroo.Stream (ConsumeM, Producer, constantProducer, runConsumeM_, unitProducer)
import Swictheroo.Stream as Stream

type Producers m =
  { download :: Producer m Int
  , reportDownload :: Producer m String
  , ping :: Producer m Boolean
  }

program :: forall m. Monad m => Producers m -> ConsumeM m Unit Unit String
program producers = Ix.do
  Stream.replace producers.download
  a <- Stream.pull

  Stream.replace producers.reportDownload
  b <- Stream.pull

  Stream.replace producers.ping
  c <- Stream.pull

  Stream.replace unitProducer

  pure $ Array.fold
    [ "Download: "
    , show a
    , ", Report: "
    , show b
    , ", Ping: "
    , show c
    ]

{-
Pseudo-code for testing:

data SyntheticEvent = Download Int | Report String | Ping Int

producers =
  { download: case _ of
    Download s -> Just s
    _ -> Nothing
  , report: ...
  , ping: ... 
  }

story =
  [ Emit (Download 7)
  , Expect Cancel
  , Emit (Report 7)
  , Expect Cancel
  , Emit (Ping 7)
  , Expect Cancel
  ] 
-}

{- How do we cancel our program?

The program would require an extra producer called "cancel".
Every time we call "Stream.replace", we would merge "cancel" with 
the respective new stream. This way, when we call "Stream.pull" we can
either receive a "Left" (which means we must cancel) or a "Right" 
(which means we have gotten our value and can keep going).
-}

{- All async effects should be avoided outside the internals of our monad!

Eg: do 
  a <- lift $ Aff.wait ...

This is *bad*, because it means we cannot receive a cancellation event. 

A better idea would be:
do 
  Stream.mapSource (\old -> merge old (fromAff (Aff.wait ...)))

  result <- Stream.pull

  case result of
    -- Our effect finished running!
    Left effectResult -> do
       -- Return to old producer
       Stream.mapSource Either.hush
       -- Do stuff with the result of the effect
       ...

    -- Try again later!
    Right otherResult -> do
       -- Handle the other result
       ... 

This does not block the other events at any point!
Although this might look complicated, it's possible to create helpers for 
common patterns like this one!

Note that in an actual production codebase we would receive
  (fromAff (Aff.wait ...)) from the exterior in order to
  allow mocking and to not tie ourselves to a specific monad.
-}

{- One cool thing about this approach is 
that simple state can be kept without the need for StateT!
(because we can simply pass around values)
-}

main :: Effect Unit
main = log (coerce result)
  where
  result :: Identity _
  result = runConsumeM_ $ program
    { download: constantProducer 3
    , reportDownload: constantProducer "foo"
    , ping: constantProducer true
    }

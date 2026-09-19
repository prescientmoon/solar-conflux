module Sky.Debug where

import Prelude

import Data.Debug (class Debug, debug, opaque_, prettyPrintWith)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Console as Console
import Effect.Unsafe (unsafePerformEffect)

-- | Like show but for stuff with debug instances.
showPretty :: forall d. Debug d => d -> String
showPretty = prettyPrintWith { compactThreshold: 6, maxDepth: Just 1000 } <<< debug

-- | Log a prettified version of a value to the console
logPretty :: forall d. Debug d => d -> Effect Unit
logPretty = showPretty >>> log

-- | A debug instance which shows more context.
-- | At repl, call `:print Loglude.Data.Debug.myDebug`
myDebug :: forall d. Debug d => d -> Effect Unit
myDebug = Console.log <<< showPretty

-- | Similar to spy but requires a debug instance.
debugSpy :: forall a. Debug a => a -> a
debugSpy a = unsafePerformEffect (a <$ myDebug a)

data Opqaue = Opqaue

instance Debug Opqaue where
  debug _ = opaque_ "opqaue"
module Io where

import Prelude hiding (bind,discard)

import Effect (Effect)
import Effect.Class.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Sync as Fs
import Test.Assert as Assert
import Unsafe.Coerce (unsafeCoerce)

class Io

pure :: forall a. a -> Io => a
pure a = a

assert :: Boolean -> (Io => Unit)
assert = Assert.assert >>> effectToIo

handleIo :: forall a. (Io => a) -> Effect a
handleIo = unsafeCoerce

effectToIo :: forall a. Effect a -> (Io => a)
effectToIo = unsafeCoerce

debugLog :: String -> Io => Unit
debugLog = Console.log >>> effectToIo

readFile :: String -> Io => String
readFile = Fs.readTextFile UTF8 >>> effectToIo 

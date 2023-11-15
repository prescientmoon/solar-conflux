module Thumbor.Internals where

import Prim.Row

type WithConfig config rest optional required
  = Union config rest optional => { | required config }

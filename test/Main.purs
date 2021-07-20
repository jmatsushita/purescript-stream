module Test.Main where

import Data.Stream
import Partial.Unsafe
import Prelude

import Control.Comonad
import Control.Comonad.Store
import Data.Lazy
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Data.Array hiding (take)

-- > iterate f x = [x, f x, f (f x), ..]
-- iterate :: (a -> a) -> a -> Stream a

stream0N :: Stream Int
stream0N = iterate (add 1) 0

streamBoom :: Stream Char
streamBoom = iterate (\_ -> unsafeCrashWith "boom") 'n'

functorLazy :: Stream Int
functorLazy = map f $ streamBoom
  where
    f _ = 0

functor :: Stream Int
functor = map (add 1) stream0N


main :: Effect Unit
main = do
  log "🍝"
  logShow $ take 3 functor == [1,2,3]
  logShow $ extract $ functor
  logShow $ extract $ extract $ duplicate $ functor
  logShow $ take 3 functorLazy == [0,0,0]
  log "You should add some tests."

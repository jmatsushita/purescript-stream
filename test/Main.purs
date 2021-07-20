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

-- data Store s a = Store a (a -> s)

type Index = Int -- Naturals

myStore :: Store Index Char
myStore = store (\i -> unsafePartial $ unsafeIndex ['a','b','c'] i) 0

-- instance extendStore :: Extend (Store s a) where
--   extend f = map f <<< duplicate
--     where
--       duplicate (Store i f) = Store i ()

main :: Effect Unit
main = do
  log "🍝"
  -- logShow $ pos myStore
  -- logShow $ peek (pos myStore) myStore
  logShow $ extract myStore
  logShow $ extract $ seek 2 myStore
  logShow $ extract $ extract $ (duplicate myStore :: Store Index (Store Index Char))
  logShow $ extract $ extend (\w -> [extract w, extract $ seek (add 1 $ pos w) w] <> ['!']) myStore 
  -- logShow $ take 3 functor == [1,2,3]
  -- logShow $ extract $ functor
  -- logShow $ extract $ extract $ duplicate $ functor
  -- logShow $ take 3 functorLazy == [0,0,0]
  log "You should add some tests."

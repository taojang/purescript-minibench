module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Performance.Minibench (bench, benchWith, benchAffWith)

loop :: Int -> Int
loop 0 = 0
loop n = loop (n - 1)

main :: Effect Unit
main = do
  testPure
  launchAff_ testAff

testPure :: Effect Unit
testPure =  do
  log "loop 10"
  benchWith 1000000 \_ -> loop 10
  log "loop 100"
  benchWith 100000 \_ -> loop 100
  log "loop 1000"
  bench \_ -> loop 1000
  log "loop 10000"
  bench \_ -> loop 10000
  log "loop 100000"
  bench \_ -> loop 100000
  log "loop 1000000"
  benchWith 100 \_ -> loop 1000000

loopAff :: Int -> Aff Int
loopAff 0 = pure 0
loopAff n = do
  pure unit
  loopAff (n - 1)

testAff :: Aff Unit
testAff = do
  liftEffect $ log "loop 10"
  benchAffWith 1000000 (loopAff 10)
  liftEffect $ log "loop 100"
  benchAffWith 100000 (loopAff 100)
  liftEffect $ log "loop 1000"
  benchAffWith 100 (loopAff 1000)

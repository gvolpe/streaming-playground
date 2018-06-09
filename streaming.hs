#!/usr/bin/env stack
{- stack
  script
  --resolver lts-11.12
  --package streaming
-}

import Streaming
import qualified Streaming.Prelude as S

sumInput :: IO (Of Int ())
sumInput = S.sum $ S.take 3 (S.readLn :: Stream (Of Int) IO ())

main :: IO ()
main = do
    rs <- sumInput
    print rs

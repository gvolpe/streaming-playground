#!/usr/bin/env stack
{- stack
   --resolver lts-11.12
   exec ghci
   --package streaming
-}

{-# LANGUAGE ScopedTypeVariables #-}

import Data.Char (toUpper)
import Data.IORef (newIORef, readIORef)
import Streaming (Stream, Of, chunksOf, concats, lift)
import qualified Streaming.Prelude as S
import Streaming.Internal( Stream (Effect, Return, Step) )

sumInput :: IO (Of Int ())
sumInput = S.sum $ S.take 3 (S.readLn :: Stream (Of Int) IO ())

uppercaseInput :: IO ()
uppercaseInput = S.stdoutLn $ S.map (map toUpper) $ S.take 2 S.stdinLn

ref :: IO ()
ref = S.print $ S.mapM readIORef $ S.mapM newIORef $ S.each [1..100::Int]

chunks :: IO ()
chunks = S.print $ concats $ chunksOf 2 $ S.each [1,2,3,4,5,6]

--pauses' = S.print $ S.delay 1.0 S.each [1..10]
pauses :: IO ()
pauses = S.print $ concats chunkies
    where chunkies :: Stream (Stream (Of Int) IO) IO ()
          chunkies = S.maps (\s -> S.delay 1.0 s) $ chunksOf 2 $ S.each [1..10]

concats' :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats' = loop where
  loop stream = case stream of
    Return r -> return r
    Effect m -> lift m >>= loop
    Step fs  -> fs >>= loop

main :: IO ()
main = do
    rs <- chunks
    print rs

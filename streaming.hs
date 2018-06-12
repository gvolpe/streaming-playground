#!/usr/bin/env stack
{- stack
   --resolver lts-11.12
   exec ghci
   --package streaming
-}

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
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

streamDelay :: Int -> Stream (Of a) IO ()
streamDelay s = liftIO $ threadDelay $ 1000000 * s

pauses :: IO ()
pauses = S.print $ concats chunkies
    where chunkies :: Stream (Stream (Of Int) IO) IO ()
          chunkies = S.maps (\s -> s <* streamDelay 2) $ chunksOf 2 $ S.each [1..10]

concats' :: (Monad m, Functor f) => Stream (Stream f m) m r -> Stream f m r
concats' = loop where
  loop stream = case stream of
    Return r -> return r
    Effect m -> lift m >>= loop
    Step fs  -> fs >>= loop

program :: IO (Of [String] ())
program = S.toList $ S.takeWhile (\c -> c /= "quit") $ S.repeatM getLine

program' :: IO (Of [String] ())
program' = S.toList $ void $ S.break (== "quit") $ S.repeatM getLine

main :: IO ()
main = do
    rs <- program
    print rs

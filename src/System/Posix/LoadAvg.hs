{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module      :  System.Posix.LoadAvg
-- There are two basic ways you can get load average on a modern Linux system.
-- First is @getloadavg (3)@ system call. The second is @\/proc\/loadavg@ file.
-- This module provides means of getting the information from both sources.
-- @\/proc\/loadavg@ provides some additional information but we ignore that.
module System.Posix.LoadAvg ( LoadAvg(..), 
                              parseLoadAvg, 
                              getLoadAvg, getLoadAvgSafe, 
                              getLoadAvgProc ) where

import Foreign
import Data.Maybe

data LoadAvg = LoadAvg { sample_1  :: !Double -- ^ sample for last 1 minute
                       , sample_5  :: !Double -- ^ sample for last 5 minutes
                       , sample_15 :: !Double -- ^ sample for last 15 minutes
                       }
             deriving (Read,Show,Eq,Ord)

foreign import ccall "getloadavg" getloadavg_C :: Ptr Double -> Int -> IO Int

-- | Discards error checking from getLoadAvgSafe. Will raise IO exception on error.
getLoadAvg :: IO LoadAvg
getLoadAvg = (fromMaybe (error "getLoadAvg: an error occured")) `fmap` getLoadAvgSafe

-- | Calls @getloadavg (3)@ to get system load average. 
--   Provides error checking, and the result may be Nothing in case of error.
--   If there is not enough data the samples will be copied from more recent samples.
getLoadAvgSafe :: IO (Maybe LoadAvg)
getLoadAvgSafe = allocaArray 3 $ \arr ->
    do
      res <- getloadavg_C arr 3
      case res of
        -- Kind of hacky, I know
        3 -> do
          [a,b,c] <- peekArray 3 arr
          return . Just $ LoadAvg a b c
        2 -> do
          [a,b] <- peekArray 2 arr
          return . Just $ LoadAvg a b b
        1 -> do
          [a] <- peekArray 1 arr
          return . Just $ LoadAvg a a a
        -- Instead of matching with _ perhaps whe should check that we got '-1' here
        _ -> return Nothing

-- | Tries to read @\/proc\/loadavg@ and parse it's output with 'parseLoadAvg'. Either may fail with IO exception. 
getLoadAvgProc :: IO LoadAvg
getLoadAvgProc = parseLoadAvg `fmap` readFile "/proc/loadavg"

-- | Tries to parse the output of @\/proc\/loadavg@. If anything goes wrong an arbitrary exception will be raised.
parseLoadAvg :: String -> LoadAvg
parseLoadAvg input = let (a:b:c:_) = map read $ words input in LoadAvg a b c


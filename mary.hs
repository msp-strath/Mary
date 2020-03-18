module Main where

import Data.Text.IO as TIO

import System.Environment

import Text.Pandoc.JSON

import Shonkier

import Mary.Pandoc
import Mary.ServePage
import Mary.ServeWeb

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    ["-pandoc"]               -> toJSONFilter process
    ["-shonkier", filename]   -> interpretShonkier filename
    ["-shonkierjs", filename] -> compileShonkier filename >>= TIO.putStrLn
    ["-page", filename]       -> servePage testConfig filename >>= TIO.putStrLn
    ["-web", pandoc, sitesRoot, user, page] -> do
      mary    <- getExecutablePath
      content <- serveWeb Config{..} sitesRoot user page
      TIO.putStrLn content
    _ -> TIO.putStr "# mary says\nI don't know what you're on about.\n\n"


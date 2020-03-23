module Main where

import Control.Exception as E

import Data.Semigroup ((<>))
import Data.Text.IO as TIO

import Text.Pandoc.JSON (toJSONFilter)

import Options.Applicative

import Shonkier

import Mary.Pandoc
import Mary.ServePage
import Mary.Find

import Paths_mary

main :: IO ()
main = customExecParser pp opts >>= \ o -> E.handle h $ case o of
  Pandoc              -> toJSONFilter process
  Shonkier filename   -> interpretShonkier filename
  Shonkierjs filename -> do
    shonkierjs <- getDataFileName "src/data-dir/Shonkier.js"
    compileShonkier shonkierjs filename >>= TIO.putStrLn
  Page filename       -> servePage testConfig filename >>= TIO.putStrLn
  Find{..}            -> maryFind user sitesRoot page
  where
    pp = prefs showHelpOnEmpty
    opts = info (optsParser <**> helper)
      ( fullDesc <> header "Mary - a content delivery and assessment engine")
    h :: SomeException -> IO ()
    h e = Prelude.putStrLn $ "mary ERROR " ++ displayException e

data Options
  = Pandoc
  | Shonkier   { filename :: String }
  | Shonkierjs { filename :: String }
  | Page       { filename :: String }
  | Find       { user      :: String
               , sitesRoot :: String
               , page      :: String
               }

optsParser :: Parser Options
optsParser = subparser
  ( command' "pandoc"
             (pure Pandoc)
             "Act as a Pandoc filter"
 <> command' "shonkier"
             (Shonkier <$> strArgument
                (metavar "FILE" <> action "file" <> help "Input Shonkier program."))
             "Interpret shonkier program"
 <> command' "shonkierjs"
             (Shonkierjs <$> strArgument
                  (metavar "FILE" <> action "file" <> help "Source Shonkier program."))
             "Compile shonkier program to javascript"
 <> command' "page"
             (Page <$> strArgument
                  (metavar "FILE" <> action "file" <> help "Input Mary file"))
             "Generate HTML from Mary file"
 <> command' "find"
             (Find <$> strArgument (metavar "USER" <> action "user" <> help "Username.")
                   <*> strArgument (metavar "ROOT" <> action "directory" <> help "Path to site root.")
                   <*> strArgument (metavar "PAGE" <> action "file" <> help "Page to serve."))
             "Find webpage and output markdown")
  where
    command' :: String -> Parser a -> String -> Mod CommandFields a
    command' label parser description =
      command label (info parser (progDesc description))

module Main where

import Control.Exception as E

import Data.List as L
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text
import Data.Text.IO as TIO

import Text.Pandoc.JSON (toJSONFilter)

import System.IO as SIO
import System.Environment

import Options.Applicative

import Shonkier

import Mary.Pandoc
import Mary.ServePage
import Mary.Find

import Paths_mary

defaultUser :: IO String
defaultUser = do
  u <- lookupEnv "LOGNAME"
  pure $ fromMaybe "Shelley" u

main :: IO ()
main = customExecParser pp opts >>= \ o -> E.handle h $ case o of
  Pandoc              -> toJSONFilter process
  Shonkier filename   -> interpretShonkier filename
  Shonkierjs filename -> do
    shonkierjs <- getDataFileName "src/data-dir/Shonkier.js"
    compileShonkier shonkierjs filename >>= TIO.putStrLn
  Page filename postString getString siteRoot username -> do
    let postArray = parseRequests (pack postString)
    let getArray' = parseRequests (pack getString)
    -- make sure there is a page
    let getArray = case lookup "page" getArray' of
          Just _  -> getArray'
          Nothing -> ("page", pack filename):getArray'
    user <- if L.null username then defaultUser else pure username
    let mary = "mary"
    let pandoc = "pandoc"
    servePage Config{..} postArray getArray filename >>= TIO.putStrLn
  Find{..}            -> do
    user' <- if L.null user then defaultUser else pure user
    maryFind sitesRoot user' page
  where
    pp = prefs showHelpOnEmpty
    opts = info (optsParser <**> helper)
      ( fullDesc <> header "Mary - a content delivery and assessment engine")
    h :: SomeException -> IO ()
    h e = SIO.hPutStrLn stderr $ "mary ERROR " ++ displayException e

data Options
  = Pandoc
  | Shonkier   { filename :: String }
  | Shonkierjs { filename :: String }
  | Page       { filename :: String
               , postArray :: String
               , getArray :: String
               , siteRoot :: String
               , user     :: String
               }
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
             (Page <$> strArgument (metavar "FILE" <> action "file" <> help "Input Mary file")
                   <*> option str (long "post" <> value ""
                                     <> metavar "STRING" <> help "POST input string (&-separated)")
                   <*> option str (long "get" <> value ""
                                    <> metavar "STRING" <> help "GET input string (&-separated)")
                   <*> option str (long "siteRoot" <> value "."
                                    <> metavar "STRING" <> action "directory" <> help "Site root.")
                   <*> option str (long "user" <> value ""
                                     <> metavar "STRING" <> action "user" <> help "Username"))
             "Generate HTML from Mary file"
 <> command' "find"
             (Find <$> option str (long "user" <> value ""
                                               <> metavar "STRING" <> action "user" <> help "Username.")
                   <*> strArgument (metavar "ROOT" <> action "directory" <> help "Path to site root.")
                   <*> strArgument (metavar "PAGE" <> action "file" <> help "Page to serve."))
             "Find webpage and output markdown")
  where
    command' :: String -> Parser a -> String -> Mod CommandFields a
    command' label parser description =
      command label (info parser (progDesc description))

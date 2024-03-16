module Main (main) where

import CmdLineOpts (Options (NormalOptions), options)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Lib (runApp)
import Options.Applicative
  ( ParserPrefs (prefColumns),
    customExecParser,
    defaultPrefs,
  )
import System.Console.Terminal.Size (Window (Window), size)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  a <- do
    b <- size
    pure $ case b of
      Nothing -> 80
      Just (Window _ w) -> w

  (NormalOptions propText printSubexpr printToStdout outputPth mergeTables gradientColor) <- customExecParser (defaultPrefs {prefColumns = a}) options

  case runApp propText printSubexpr mergeTables gradientColor of
    Left e -> do
      TIO.hPutStrLn stderr $ T.pack $ errorBundlePretty e
      exitWith $ ExitFailure 1
    Right res ->
      if printToStdout
        then TIO.putStrLn res
        else TIO.writeFile outputPth res

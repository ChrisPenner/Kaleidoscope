module Kaleidoscope.Repl where

import Control.Monad.Trans
import System.Console.Haskeline

import qualified Data.Text as T
import Kaleidoscope.Parser

process :: T.Text -> IO ()
process line = do
  let res = parseToplevel line
  case res of
    Left err -> putStrLn err
    Right ex -> mapM_ print ex

repl :: IO ()
repl = runInputT defaultSettings loop
  where
  loop = do
    minput <- fmap T.pack <$> getInputLine "$> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> (liftIO $ process input) >> loop

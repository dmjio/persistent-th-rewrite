module Main where

import System.Environment
import System.Exit
import GHC.Plugin.PersistentThRewrite

import Language.Haskell.GHC.ExactPrint.Parsers
import GhcPlugins

main :: IO ()
main = do
  args <- getArgs
  case args of
    original:input:output:_ ->
      go input output

go :: String -> String -> IO ()
go input output = do
  result <- parseModule input
  case result of
    Right (_, L _ s) -> do
      let n = rewriteModule s
      writeFile output $ showSDocUnsafe (ppr n)
    Left s -> print s >> exitFailure


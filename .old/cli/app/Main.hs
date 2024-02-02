module Main where

import Codchi
import Codchi.CLI
import Codchi.Platform
import Main.Utf8

main :: IO ()
main = withUtf8 $ withStderrLogging $ runCodchi (cli =<< parseCmd)

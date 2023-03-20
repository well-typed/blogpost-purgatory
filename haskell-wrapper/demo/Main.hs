{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Certificate

main :: IO ()
main = print =<< selfSigned ["John Smith"]
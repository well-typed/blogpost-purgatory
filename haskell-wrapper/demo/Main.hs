{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Certificate
import Foreign.Rust.Serialisation.Raw

main :: IO ()
main = do
    (cert, pkey) <- genSelfSigned ["John Smith"]
    print $ certificateSubject cert
    print $ rawSize pkey
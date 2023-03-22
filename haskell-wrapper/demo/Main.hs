{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Annotated (annotate)
import Foreign.Rust.Serialisation.Raw

import Certificate

main :: IO ()
main = do
    (cert, pkey) <- genSelfSigned ["John Smith"]
    print $ cert
    print $ annotate cert
    print $ certificateSubject cert
    print $ rawSize pkey
    print $ exampleKey 0
    print $ exampleKey 0
    print $ exampleKey 1
    print $ toPem pkey
    print $ fromPem (toPem pkey)
    print $ fromPem ""
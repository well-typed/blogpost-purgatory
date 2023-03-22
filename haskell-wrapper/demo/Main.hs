{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Mem

import Data.Annotated (annotate)
import Foreign.Rust.Serialisation.Raw

import Certificate
import Handle

main :: IO ()
main = do
    putStrLn "# Marshalling\n"

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

    putStrLn "\n# Without marshalling\n"

    h0 <- newHandle
    h1 <- newHandle
    h2 <- newHandle
    print (h0, h1, h2)
    performMajorGC
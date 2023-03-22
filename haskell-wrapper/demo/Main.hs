{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Certificate
import Foreign.Rust.Serialisation.Raw

main :: IO ()
main = do
    (cert, pkey) <- genSelfSigned ["John Smith"]
    print $ cert
    print $ certificateSubject cert
    print $ rawSize pkey
    print $ exampleKey 0
    print $ exampleKey 0
    print $ exampleKey 1
    print $ toPem pkey
    print $ fromPem (toPem pkey)
    print $ fromPem ""
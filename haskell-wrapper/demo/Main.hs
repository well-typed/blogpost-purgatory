{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.Mem

import Data.Annotated

import Foreign.Rust.Serialisation.Raw

import C.Color.Native qualified as Native
import C.GettingStarted
import Certificate
import Color.Foreign qualified as Foreign
import Handle

main :: IO ()
main = do
    putStrLn "\n# Getting started\n"

    print $ rustWrapperAdd 1 2

    putStrLn "\n# Marshalling\n"

    (cert, pkey) <- selfSigned ["example.com"]
    print $ cert
    print $ annotate [Just cert]
    print $ dropAnnotation @[Maybe Certificate] $ annotate [Just cert]
    print $ certificateSubject cert
    print $ certificateSubject "MIIBVDCB+qADAgECAgkAyqwkJjeqBgAwCgYIKoZIzj0EAwIwITEfMB0GA1UEAwwWcmNnZW4gc2VsZiBzaWduZWQgY2VydDAgFw03NTAxMDEwMDAwMDBaGA80MDk2MDEwMTAwMDAwMFowITEfMB0GA1UEAwwWcmNnZW4gc2VsZiBzaWduZWQgY2VydDBZMBMGByqGSM49AgEGCCqGSM49AwEHA0IABEDhs6AFRqaPY3YL2rQzFHSU7QX4OfYBwDw7Eg1C1IJekhfb1FDN4Gx0vx6Nt/wfauFk7ngyCg6AIOktt6bIMu+jGTAXMBUGA1UdEQQOMAyCCkpvaG4gU21pdGgwCgYIKoZIzj0EAwIDSQAwRgIhAJuPoT7BOnDK48hC6Scgbd5IS1YERUh5LuYJVRdRePc0AiEAxlLn6uUvhIMlJGVOdKpH0VbRzZKIe10k6QFHvRJW9Pc="
    print $ rawSize pkey
    print $ exampleKey 0
    print $ exampleKey 0
    print $ exampleKey 1
    print $ toPem pkey
    print $ fromPem (toPem pkey)
    print $ fromPem ""

    putStrLn "\n# External buffer\n"

    print $ toPemExternal pkey

    putStrLn "\n# Colors\n"

    print $ Native.red
    print $ Foreign.red
    print $ Foreign.colorToJSON $ Foreign.red
    print $ Foreign.colorFromJSON $ Foreign.colorToJSON $ Foreign.red

    putStrLn "\n# Without marshalling\n"

    h0 <- newHandle
    h1 <- newHandle
    h2 <- newHandle
    print (h0, h1, h2)
    performMajorGC
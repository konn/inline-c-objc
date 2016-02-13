{-# LANGUAGE DataKinds, ForeignFunctionInterface, GADTs, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, QuasiQuotes, TemplateHaskell, TypeFamilies         #-}
module Main where
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Language.ObjC.Inline as C

C.context C.objcCtx
C.import_ "<AppKit/AppKit.h>"
C.import_ "<Foundation/Foundation.h>"

nsLog :: Text -> IO ()
nsLog txt = [C.block| void { NSLog(@"%@", $txt:txt); } |]

main :: IO ()
main = nsLog =<< [C.exp'| NSString * { @"Hay, I'm Cocoa! だよ！" } |]

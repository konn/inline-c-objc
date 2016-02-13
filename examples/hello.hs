{-# LANGUAGE DataKinds, ForeignFunctionInterface, GADTs, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, QuasiQuotes, TemplateHaskell, TypeFamilies         #-}
module Main where
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Language.C.Inline    as C
import qualified Language.C.Types     as C
import           Language.ObjC.Inline

C.context objcCtx
import_ "<AppKit/AppKit.h>"
import_ "<Foundation/Foundation.h>"

nsLog :: Text -> IO ()
nsLog txt = [C.block| void { NSLog(@"%@", $txt:txt); } |]

main :: IO ()
main = nsLog "Hay, I'm Cocoa! だよ！"

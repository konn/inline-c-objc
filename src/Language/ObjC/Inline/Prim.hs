module Language.ObjC.Inline.Prim (objcPrimCtx, import_) where
import Language.C.Inline.Context (Context(..))
import qualified Language.C.Inline as C
import qualified Data.Map as M
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH
import Foreign (Ptr)
import Data.Word (Word16)
import Language.Haskell.TH (DecsQ)
import Language.C.Inline (verbatim)
import Data.Monoid ((<>))
import Foreign.C (CInt, CULong, CLong)

objcPrimCtx :: [(C.TypeSpecifier, TH.TypeQ)] -> Context
objcPrimCtx dic = C.baseCtx  <>
              Context { ctxFileExtension = Just "m"
                      , ctxTypesTable = M.fromList dic <> objcPrimTable
                      , ctxAntiQuoters = M.empty
                      , ctxOutput = Nothing
                      }

objcPrimTable :: M.Map C.TypeSpecifier TH.TypeQ
objcPrimTable = M.fromList [(C.TypeName "id", [t|Ptr ()|])
                           ,(C.TypeName "UInt16", [t|Word16|])
                           ,(C.TypeName "NSUInteger", [t|CULong|])
                           ,(C.TypeName "NSInteger", [t|CLong|])
                           ,(C.TypeName "BOOL", [t|CInt|])
                           ]

import_ :: String -> DecsQ
import_ s
  | null s        = fail "inline-c-objc: empty string (import_)"
  | head s == '<' = verbatim $ "#import " ++ s
  | otherwise     = verbatim $ "#import \"" ++ s ++ "\""

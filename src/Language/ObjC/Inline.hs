{-# LANGUAGE DataKinds, EmptyDataDecls, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface, GADTs, GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE KindSignatures, LambdaCase, MultiParamTypeClasses              #-}
{-# LANGUAGE OverloadedStrings, PolyKinds, QuasiQuotes, RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeFamilies             #-}
{-# LANGUAGE TypeOperators, UndecidableInstances, ViewPatterns              #-}
module Language.ObjC.Inline (objcCtx, objcCtxWithClasses, import_, ObjC(..), attain, (:>),
                             upcast, NSString, fromNSString, safeCastObjC, exp,
                             fromNSArray, fromNSArray', toNSArray, toNSArray',
                             Class(..), Object(..), fromObjC, toObjC, toObjC',
                             NSArray, NSArray', NSData, description, defClass, defStruct,
                             -- * Re-export
                             C.context,
                             -- * Redefined quotes
                             exp, block, pure, exp', block', pure'
                            ) where
import Language.ObjC.Inline.Prim

import           Control.Lens                        (iforM_)
import           Control.Monad                       (forM)
import           Data.ByteString                     (ByteString)
import           Data.ByteString                     (packCStringLen)
import           Data.ByteString                     (useAsCStringLen)
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Unsafe              as BS
import qualified Data.Map                            as M
import           Data.Monoid                         ((<>))
import           Data.Proxy                          (Proxy (Proxy))
import           Data.String                         (fromString)
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.Foreign                   as T
import           Foreign                             (ForeignPtr, FunPtr, Ptr)
import           Foreign                             (castPtr, newForeignPtr)
import           Foreign.ForeignPtr                  (FinalizerPtr)
import           Foreign.Storable                    (Storable)
import           GHC.TypeLits                        (KnownSymbol, Symbol)
import           GHC.TypeLits                        (symbolVal)
import qualified Language.C.Inline                   as C
import           Language.C.Inline.Context           (AntiQuoter (..))
import           Language.C.Inline.Context           (Context (..))
import           Language.C.Inline.Context           (Purity (IO, Pure))
import           Language.C.Inline.Context           (SomeAntiQuoter (..))
import           Language.C.Inline.Context           (TypesTable, convertType)
import           Language.C.Inline.HaskellIdentifier (HaskellIdentifier)
import           Language.C.Inline.HaskellIdentifier (mangleHaskellIdentifier)
import           Language.C.Inline.HaskellIdentifier (unHaskellIdentifier)
import qualified Language.C.Inline.Internal          as P
import qualified Language.C.Types                    as C
import qualified Language.C.Types                    as T
import           Language.Haskell.TH                 (Type (AppT, ConT), TypeQ)
import           Language.Haskell.TH                 (litT, pprint, strTyLit)
import           Language.Haskell.TH                 (stringL)
import           Language.Haskell.TH                 (runIO)
import           Language.Haskell.TH                 (ppr)
import qualified Language.Haskell.TH                 as TH
import           Language.Haskell.TH.Quote           (QuasiQuoter (..))
import           Prelude                             hiding (exp, pure)
import           System.IO.Unsafe                    (unsafePerformIO)
import qualified Text.Parser.Token                   as Parser

newtype ObjC (a :: k) = ObjC { runObjC :: Ptr (ObjC a) }
                      deriving (Show, Eq, Ord, Storable)

defClass :: String -> (C.TypeSpecifier, TypeQ)
defClass s = (C.TypeName (fromString s), [t|ObjC $(litT $ strTyLit s)|])
defStruct :: String -> TypeQ -> (C.TypeSpecifier, TypeQ)
defStruct s t = (C.Struct (fromString s), t)

objcDefTable =
  [defClass "NSString"
  ,defClass "NSObject"
  ,defClass "NSArray"
  ,defClass "NSData"
  ,defClass "NSMutableArray"]


C.context (C.baseCtx <> objcPrimCtx [(C.TypeName "NSString", [t|ObjC "NSString"|])
  ,(C.TypeName "NSObject", [t|ObjC "NSObject"|])
  ,(C.TypeName "NSArray",  [t|ObjC "NSArray"|])
  ,(C.TypeName "NSData", [t|ObjC "NSData"|])
  ,(C.TypeName "NSMutableArray", [t|ObjC "NSMutableArray"|])])
import_ "<Foundation/Foundation.h>"

class a :> b

class Class a where
  className :: proxy a -> C.CIdentifier

instance KnownSymbol s => Class s where
  className pxy = fromString $ symbolVal pxy

class Class a => Object a where
  type Haskell a :: *
  fromObjC_ ::  Ptr (ObjC a) -> IO (Haskell a)
  toObjC_   ::  proxy a -> Haskell a -> IO (Ptr (ObjC a))

myGenercQuote :: Bool           -- ^ to demarshal or not
              -> Purity         -- ^ purity
              -> (TypeQ -> T.Type T.CIdentifier
                        -> [(T.CIdentifier, T.Type T.CIdentifier)]
                        -> String
                        -> TH.Q TH.Exp)
              -> QuasiQuoter
myGenercQuote demarshal purity build =
  P.genericQuote purity $ \typeQ cType cxt src -> do
  ctx <- P.getContext
  let viewer tpq = tpq >>= \case
        AppT (ConT obc) _ | obc == ''ObjC -> return True
        _ -> return False
  needsObjC <- case cType of
    C.Ptr _ (C.TypeSpecifier _ spec) -> maybe (return False) viewer $ M.lookup spec (ctxTypesTable ctx)
    _ -> return False
  ans <- build typeQ cType cxt src
  let demarsh | demarshal = [| (fromObjC . ObjC =<<) |]
              | otherwise = [| fmap ObjC |]
  if needsObjC then [|$demarsh $(return ans)|] else return ans

-- | ObjC suited qqs.
exp, block, pure :: QuasiQuoter
exp = myGenercQuote False IO $ P.inlineExp TH.Safe
block = myGenercQuote False IO $ P.inlineItems TH.Safe
pure = myGenercQuote False Pure $ P.inlineExp TH.Safe

-- | ObjC suited qqs with demarshalling.
exp', block', pure' :: QuasiQuoter
exp' = myGenercQuote True IO $ P.inlineExp TH.Safe
block' = myGenercQuote True IO $ P.inlineItems TH.Safe
pure' = myGenercQuote True Pure $ P.inlineExp TH.Safe

fromObjC :: Object a => ObjC a -> IO (Haskell a)
fromObjC (ObjC ptr) = fromObjC_ ptr

toObjC :: Object a => proxy a -> Haskell a -> IO (ObjC a)
toObjC pxy a = ObjC <$> toObjC_ pxy a

toObjC' :: forall a. Object a => Haskell a -> IO (ObjC a)
toObjC' = toObjC (Proxy :: Proxy a)

instance Object "NSData" where
  type Haskell "NSData" = ByteString
  fromObjC_ ptr = do
    len   <- [C.exp| NSUInteger { [($(NSData *ptr)) length] } |]
    bytes <- [C.exp| const char * { (const char *)[($(NSData *ptr)) bytes] } |]
    packCStringLen (bytes, fromIntegral len)
  toObjC_ _ txt = useAsCStringLen txt $ \ (bytes, fromIntegral -> len) ->
    [C.exp| NSData * { [NSData dataWithBytesNoCopy: $(char * bytes)
                                            length: $(int len)] } |]

instance Object "NSString" where
  type Haskell "NSString" = Text
  fromObjC_ ptr = fromNSString (ObjC ptr)
  toObjC_ _ txt = runObjC <$> mkNSString txt

data NSArrayEl a
data NSMutableArrayEl a
instance "NSObject" :> (b :: k)
instance NSMutableArrayEl a :> NSMutableArrayEl a
instance (a :> b) => NSArrayEl a :> NSArrayEl b

type NSArray' a = ObjC (NSArrayEl a)
type NSMutableArray' a = ObjC (NSMutableArrayEl a)
type NSArray = ObjC "NSArray"
type NSMutableArray = ObjC "NSMutableArray"
type NSObject = ObjC "NSObject"

upcast :: (a :> b) => ObjC b -> ObjC a
upcast (ObjC a) =  ObjC $ castPtr a

type NSString = ObjC "NSString"
type NSData = ObjC "NSData"

objcCtxWithClasses :: [(C.TypeSpecifier, TypeQ)] -> Context
objcCtxWithClasses cls = mempty { ctxAntiQuoters = M.fromList [("txt", SomeAntiQuoter txtAntiQuoter)
                                                              ,("raw", SomeAntiQuoter rawAQ)
                                                              ,("obj", SomeAntiQuoter objAQ)
                                                              ]
                                } <> objcPrimCtx (objcDefTable ++ cls)

objcCtx :: Context
objcCtx = objcCtxWithClasses []

mkNSString :: Text -> IO NSString
mkNSString txt = ObjC <$> T.withCStringLen txt (\ (cstr, _len) ->
  [C.exp| NSString * { [NSString stringWithCString: $(char *cstr) encoding:NSUTF8StringEncoding] } |])

attain :: Ptr () -> IO (ObjC s)
attain = return . ObjC . castPtr

convertType_ :: String -> Purity -> TypesTable -> C.Type C.CIdentifier -> TH.Q TH.Type
convertType_ err purity cTypes cTy = do
  mbHsType <- convertType purity cTypes cTy
  case mbHsType of
    Nothing -> fail $ "Cannot convert C type (" ++ err ++ ")"
    Just hsType -> return hsType

convertObjCType :: String -> Purity -> TypesTable -> C.Type C.CIdentifier -> TH.Q TH.Type
convertObjCType err purity cTypes (C.Ptr [] cTy) = do
  mbHsType <- convertType purity cTypes cTy
  case mbHsType of
    Nothing -> fail $ "Cannot convert C type (" ++ err ++ ")"
    Just hsType -> return hsType
convertObjCType err purity cTypes ty =
  fail $ "ObjC type conversion failed: " ++ show (err, purity, ty)

rawAQ :: AntiQuoter HaskellIdentifier
rawAQ = AntiQuoter
  { aqParser = do
       cTy <- Parser.parens C.parseParameterDeclaration
       case C.parameterDeclarationId cTy of
         Nothing -> fail "Every captured function must be named (funCtx)"
         Just hId -> do
           let cId = mangleHaskellIdentifier hId
           cTy' <- deHaskellifyCType $ C.parameterDeclarationType cTy
           return (cId, cTy', hId)
  , aqMarshaller = \p ctys cty cId -> do
       hsTy <- convertObjCType "objcCtx" p ctys cty
       hsExp <- getHsVariable "objcCtx" cId
       expq <- [| \cont -> cont (runObjC $(return hsExp)) |]
       return (ptrT hsTy, expq)
       }

objAQ :: AntiQuoter HaskellIdentifier
objAQ = AntiQuoter
  { aqParser = do
       cTy <- Parser.parens C.parseParameterDeclaration
       case C.parameterDeclarationId cTy of
         Nothing -> fail "Every captured function must be named (funCtx)"
         Just hId -> do
           let cId = mangleHaskellIdentifier hId
           cTy' <- deHaskellifyCType $ C.parameterDeclarationType cTy
           return (cId, cTy', hId)
  , aqMarshaller = \p ctys cty cId -> do
       hsTy <- convertObjCType "objcCtx" p ctys cty
       hsExp <- getHsVariable "objcCtx" cId
       expq <- [| \cont -> cont =<< (toObjC_ (undefined :: $(return hsTy)) $(return hsExp)) |]
       return (ptrT hsTy, expq)
       }

ptrT = AppT (ConT ''Ptr)

deHaskellifyCType
  :: C.CParser HaskellIdentifier m
  => C.Type HaskellIdentifier -> m (C.Type C.CIdentifier)
deHaskellifyCType = traverse $ \hId -> do
  case C.cIdentifierFromString (unHaskellIdentifier hId) of
    Left err -> fail $ "Illegal Haskell identifier " ++ unHaskellIdentifier hId ++
                       " in C type:\n" ++ err
    Right x -> return x

txtAntiQuoter :: AntiQuoter HaskellIdentifier
txtAntiQuoter = AntiQuoter
  { aqParser = do
       hId <- C.parseIdentifier
       let cId = mangleHaskellIdentifier hId
       return (cId, C.Ptr [] $ C.TypeSpecifier mempty (C.TypeName "NSString"), hId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
       case cTy of
         C.Ptr [] (C.TypeSpecifier _ (C.TypeName cls))
           | cls `elem` stringClasses -> do
               hsTy   <- [t| ObjC "NSString" |]
               hsExp  <- getHsVariable "objcCtx" cId
               hsExp' <- [| \cont -> cont =<< mkNSString $(return hsExp)
                          |]
               return (hsTy, hsExp')
         _ -> fail "impossible: got type different from NSString * or NSMutableString *. (objcCtx)"
  }

toNSArray' :: forall proxy a. Object a => proxy a -> [Haskell a] -> IO (NSArray' a)
toNSArray' pxy els = do
  let len = fromIntegral $ length els
  arr <- [C.exp| NSMutableArray *{ [NSMutableArray arrayWithCapacity: $(int len)] } |]
  iforM_ els $ \(fromIntegral -> i) e -> do
    ob <- castPtr <$> toObjC_ pxy e
    [C.block| void { [$(NSMutableArray *arr) insertObject: $(NSObject *ob) atIndex: $(int i)]; } |]
  return $ ObjC $ castPtr arr

toNSArray :: Object a => proxy a -> [Haskell a] -> IO NSArray
toNSArray pxy els = ObjC . castPtr . runObjC <$> toNSArray' pxy els

fromNSArray' :: forall a. Object a => NSArray' a -> IO [Haskell a]
fromNSArray' (ObjC (castPtr -> arr)) = do
  len <- [C.exp| int { $(NSArray *arr).count } |]
  forM [0 .. len - 1] $ \i -> do
    ptr <- [C.exp| NSObject * { [($(NSArray *arr)) objectAtIndex: $(int i)] } |]
    fromObjC_ (castPtr ptr :: Ptr (ObjC a))

fromNSArray :: forall proxy a. Object a => proxy a -> NSArray -> IO [Maybe (Haskell a)]
fromNSArray pxy (ObjC (castPtr -> arr)) = do
  len <- [C.exp| int { $(NSArray *arr).count } |]
  forM [0 .. len - 1] $ \i -> do
    ptr <- [C.exp| NSObject * { [($(NSArray *arr)) objectAtIndex: $(int i)] } |]
    safeCastObjC pxy (ObjC ptr)

instance Class a => Class (NSArrayEl a) where
  className _ = "NSArray"

instance Object a => Object (NSArrayEl a) where
  type Haskell (NSArrayEl a) = [Haskell a]
  fromObjC_   = fromNSArray' . ObjC
  toObjC_ pxy a = runObjC <$> toNSArray' (Proxy :: Proxy a) a

arrAntiQuoter :: AntiQuoter HaskellIdentifier
arrAntiQuoter = AntiQuoter
  { aqParser = do
       cTy <- Parser.parens C.parseParameterDeclaration
       case C.parameterDeclarationId cTy of
         Nothing -> fail "Every captured function must be named (funCtx)"
         Just hId -> do
           let cId = mangleHaskellIdentifier hId
           cTy' <- deHaskellifyCType $ C.parameterDeclarationType cTy
           return (cId, cTy', hId)
  , aqMarshaller = \p ctys cty cId -> do
       elTy   <- convertType_ "objcCtx" p ctys cty
       hsTy   <- [t| ObjC (NSArrayEl $(return elTy)) |]
       hsExp  <- getHsVariable "objcCtx" cId
       hsExp' <- [| \cont -> cont =<< toNSArray' (Proxy :: Proxy $(return elTy)) $(return hsExp)|]
       return (hsTy, hsExp')
  }

stringClasses :: [C.CIdentifier]
stringClasses = ["NSString", "NSMutableString"]

fromNSString :: NSString -> IO Text
fromNSString (ObjC (castPtr -> ptr)) =
  T.decodeUtf8 <$> (BS.unsafePackCString =<< [C.exp|const char * { [($(NSString *ptr)) cStringUsingEncoding: NSUTF8StringEncoding] } |])

description :: NSObject -> IO Text
description (ObjC obj) =
  fromObjC_ =<< [C.exp| NSString * { [($(NSObject *obj)) description] } |]

safeCastObjC :: forall proxy a b. (Object a)
             => proxy a -> ObjC b -> IO (Maybe (Haskell a))
safeCastObjC p (ObjC (castPtr -> a)) = do
  let ident = className p
  ObjC cls <- mkNSString $ T.pack $ C.unCIdentifier ident
  isa <- [C.exp| BOOL { [($(NSObject *a)) isKindOfClass: NSClassFromString($(NSString *cls))]  } |]
  if isa == 1
    then Just <$> fromObjC_ (castPtr a :: Ptr (ObjC a))
    else return Nothing

-- Utils (stolen from inline-c)

getHsVariable :: String -> HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE LambdaCase                 #-}

module HiFileParser
    ( Interface(..)
    , List(..)
    , Dictionary(..)
    , Module(..)
    , Usage(..)
    , Dependencies(..)
    , getInterface
    , fromFile
    ) where

{- HLINT ignore "Reduce duplication" -}

import           Control.Monad                 (replicateM, replicateM_)
import           Data.Binary                   (Word64,Word32,Word8)
import qualified Data.Binary.Get as G          (Get, Decoder (..), bytesRead,
                                                getByteString, getInt64be,
                                                getWord32be, getWord64be,
                                                getWord8, lookAhead,
                                                runGetIncremental, skip)
import           Data.Bool                     (bool)
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.Char                     (chr)
import           Data.Functor                  (void, ($>))
import           Data.List                     (find)
import           Data.Maybe                    (catMaybes)
import           Data.Semigroup                ((<>))
import qualified Data.Vector                   as V
import           GHC.IO.IOMode                 (IOMode (..))
import           Numeric                       (showHex)
import           RIO.ByteString                as B (ByteString, hGetSome, null)
import           RIO                           (Int64,Generic, NFData)
import           System.IO                     (withBinaryFile)
import           Data.Bits                     (FiniteBits(..),testBit,
                                                unsafeShiftL,(.|.),clearBit,
                                                complement)
import           Control.Monad.State
import qualified Debug.Trace

newtype IfaceGetState = IfaceGetState
  { useLEB128 :: Bool -- ^ Use LEB128 encoding for numbers
  }

type Get a = StateT IfaceGetState G.Get a

enableDebug :: Bool
enableDebug = False

traceGet :: String -> Get ()
traceGet s
  | enableDebug = Debug.Trace.trace s (return ())
  | otherwise    = return ()

traceShow :: Show a => String -> Get a -> Get a
traceShow s g
  | not enableDebug = g
  | otherwise = do
    a <- g
    traceGet (s ++ " " ++ show a)
    return a

runGetIncremental :: Get a -> G.Decoder a
runGetIncremental g = G.runGetIncremental (evalStateT g emptyState)
  where
    emptyState = IfaceGetState False

getByteString :: Int -> Get ByteString
getByteString i = lift (G.getByteString i)

getWord8 :: Get Word8
getWord8 = lift G.getWord8

bytesRead :: Get Int64
bytesRead = lift G.bytesRead

skip :: Int -> Get ()
skip = lift . G.skip

uleb :: Get a -> Get a -> Get a
uleb f g = do
  c <- gets useLEB128
  if c then f else g

getWord32be :: Get Word32
getWord32be = uleb getULEB128 (lift G.getWord32be)

getWord64be :: Get Word64
getWord64be = uleb getULEB128 (lift G.getWord64be)

getInt64be :: Get Int64
getInt64be = uleb getSLEB128 (lift G.getInt64be)

lookAhead :: Get b -> Get b
lookAhead g = do
  s <- get
  lift $ G.lookAhead (evalStateT g s)

getPtr :: Get Word32
getPtr = lift G.getWord32be

type IsBoot = Bool

type ModuleName = ByteString

newtype List a = List
    { unList :: [a]
    } deriving newtype (Show, NFData)

newtype Dictionary = Dictionary
    { unDictionary :: V.Vector ByteString
    } deriving newtype (Show, NFData)

newtype Module = Module
    { unModule :: ModuleName
    } deriving newtype (Show, NFData)

newtype Usage = Usage
    { unUsage :: FilePath
    } deriving newtype (Show, NFData)

data Dependencies = Dependencies
    { dmods    :: List (ModuleName, IsBoot)
    , dpkgs    :: List (ModuleName, Bool)
    , dorphs   :: List Module
    , dfinsts  :: List Module
    , dplugins :: List ModuleName
    } deriving (Show, Generic)
instance NFData Dependencies

data Interface = Interface
    { deps  :: Dependencies
    , usage :: List Usage
    } deriving (Show, Generic)
instance NFData Interface

-- | Read a block prefixed with its length
withBlockPrefix :: Get a -> Get a
withBlockPrefix f = getPtr *> f

getBool :: Get Bool
getBool = toEnum . fromIntegral <$> getWord8

getString :: Get String
getString = fmap (chr . fromIntegral) . unList <$> getList getWord32be

getMaybe :: Get a -> Get (Maybe a)
getMaybe f = bool (pure Nothing) (Just <$> f) =<< getBool

getList :: Get a -> Get (List a)
getList f = do
  use_uleb <- gets useLEB128
  if use_uleb
    then do
      l <- (getSLEB128 :: Get Int64)
      List <$> replicateM (fromIntegral l) f
    else do
      i <- getWord8
      l <-
          if i == 0xff
              then getWord32be
              else pure (fromIntegral i :: Word32)
      List <$> replicateM (fromIntegral l) f

getTuple :: Get a -> Get b -> Get (a, b)
getTuple f g = (,) <$> f <*> g

getByteStringSized :: Get ByteString
getByteStringSized = do
    size <- getInt64be
    getByteString (fromIntegral size)

getDictionary :: Int -> Get Dictionary
getDictionary ptr = do
    offset <- bytesRead
    skip $ ptr - fromIntegral offset
    size <- fromIntegral <$> getInt64be
    traceGet ("Dictionary size: " ++ show size)
    dict <- Dictionary <$> V.replicateM size getByteStringSized
    traceGet ("Dictionary: " ++ show dict)
    return dict

getCachedBS :: Dictionary -> Get ByteString
getCachedBS d = go =<< traceShow "Dict index:" getWord32be
  where
    go i =
        case unDictionary d V.!? fromIntegral i of
            Just bs -> pure bs
            Nothing -> fail $ "Invalid dictionary index: " <> show i

-- | Get Fingerprint
getFP' :: Get String
getFP' = do
  x <- getWord64be
  y <- getWord64be
  return (showHex x (showHex y ""))

getFP :: Get ()
getFP = void getFP'

getInterface721 :: Dictionary -> Get Interface
getInterface721 d = do
    void getModule
    void getBool
    replicateM_ 2 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = getCachedBS d *> (Module <$> getCachedBS d)
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        pure (List [])
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface741 :: Dictionary -> Get Interface
getInterface741 d = do
    void getModule
    void getBool
    replicateM_ 3 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = getCachedBS d *> (Module <$> getCachedBS d)
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        pure (List [])
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> getString <* getWord64be <* getWord64be
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface761 :: Dictionary -> Get Interface
getInterface761 d = do
    void getModule
    void getBool
    replicateM_ 3 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = getCachedBS d *> (Module <$> getCachedBS d)
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        pure (List [])
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> getString <* getWord64be <* getWord64be
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface781 :: Dictionary -> Get Interface
getInterface781 d = do
    void getModule
    void getBool
    replicateM_ 3 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = getCachedBS d *> (Module <$> getCachedBS d)
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        pure (List [])
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> getString <* getFP
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface801 :: Dictionary -> Get Interface
getInterface801 d = do
    void getModule
    void getWord8
    replicateM_ 3 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = getCachedBS d *> (Module <$> getCachedBS d)
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        pure (List [])
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> getString <* getFP
                3 -> getModule *> getFP $> Nothing
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface821 :: Dictionary -> Get Interface
getInterface821 d = do
    void getModule
    void $ getMaybe getModule
    void getWord8
    replicateM_ 3 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = do
        idType <- getWord8
        case idType of
            0 -> void $ getCachedBS d
            _ ->
                void $
                getCachedBS d *> getList (getTuple (getCachedBS d) getModule)
        Module <$> getCachedBS d
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        pure (List [])
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> getString <* getFP
                3 -> getModule *> getFP $> Nothing
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface841 :: Dictionary -> Get Interface
getInterface841 d = do
    void getModule
    void $ getMaybe getModule
    void getWord8
    replicateM_ 5 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = do
        idType <- getWord8
        case idType of
            0 -> void $ getCachedBS d
            _ ->
                void $
                getCachedBS d *> getList (getTuple (getCachedBS d) getModule)
        Module <$> getCachedBS d
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        pure (List [])
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> getString <* getFP
                3 -> getModule *> getFP $> Nothing
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface861 :: Dictionary -> Get Interface
getInterface861 d = do
    void getModule
    void $ getMaybe getModule
    void getWord8
    replicateM_ 6 getFP
    void getBool
    void getBool
    Interface <$> getDependencies <*> getUsage
  where
    getModule = do
        idType <- getWord8
        case idType of
            0 -> void $ getCachedBS d
            _ ->
                void $
                getCachedBS d *> getList (getTuple (getCachedBS d) getModule)
        Module <$> getCachedBS d
    getDependencies =
        withBlockPrefix $
        Dependencies <$> getList (getTuple (getCachedBS d) getBool) <*>
        getList (getTuple (getCachedBS d) getBool) <*>
        getList getModule <*>
        getList getModule <*>
        getList (getCachedBS d)
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- getWord8
            case usageType of
                0 -> getModule *> getFP *> getBool $> Nothing
                1 ->
                    getCachedBS d *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> getString <* getFP
                3 -> getModule *> getFP $> Nothing
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface8101 :: Dictionary -> Get Interface
getInterface8101 d = do
    void $ traceShow "Module:" getModule
    void $ traceShow "Sig:" $ getMaybe getModule
    void getWord8
    replicateM_ 6 getFP
    void getBool
    void getBool
    Interface <$> traceShow "Dependencies:" getDependencies <*> traceShow "Usage:" getUsage
  where
    getModule = do
        idType <- traceShow "Unit type:" getWord8
        case idType of
            0 -> void $ getCachedBS d
            1 ->
                void $
                getCachedBS d *> getList (getTuple (getCachedBS d) getModule)
            _ -> fail $ "Invalid unit type: " <> show idType
        Module <$> getCachedBS d
    getDependencies =
        withBlockPrefix $
        Dependencies
          <$> getList (getTuple (getCachedBS d) getBool)
          <*> getList (getTuple (getCachedBS d) getBool)
          <*> getList getModule
          <*> getList getModule
          <*> getList (getCachedBS d)
    getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
      where
        go :: Get (Maybe Usage)
        go = do
            usageType <- traceShow "Usage type:" getWord8
            case usageType of
                0 -> traceShow "Module:" getModule *> getFP *> getBool $> Nothing
                1 ->
                    traceShow "Home module:" (getCachedBS d) *> getFP *> getMaybe getFP *>
                    getList (getTuple (getWord8 *> getCachedBS d) getFP) *>
                    getBool $> Nothing
                2 -> Just . Usage <$> traceShow "File:" getString <* traceShow "FP:" getFP'
                3 -> getModule *> getFP $> Nothing
                _ -> fail $ "Invalid usageType: " <> show usageType

getInterface :: Get Interface
getInterface = do
    let enableLEB128 = modify (\c -> c { useLEB128 = True})

    magic <- lookAhead getWord32be >>= \case
        -- normal magic
        0x1face      -> getWord32be
        0x1face64    -> getWord32be
        m            -> do
          -- GHC 8.10 mistakenly encoded header fields with LEB128
          -- so it gets special treatment
          lookAhead (enableLEB128 >> getWord32be) >>= \case
            0x1face      -> enableLEB128 >> getWord32be
            0x1face64    -> enableLEB128 >> getWord32be
            _            -> fail $ "Invalid magic: " <> showHex m ""

    traceGet ("Magic: " ++ showHex magic "")

    -- empty field (removed in 9.0...)
    case magic of
        0x1face      -> do
          e <- lookAhead getWord32be
          if e == 0
            then void getWord32be
            else enableLEB128 -- > 9.0
        0x1face64    -> do
          e <- lookAhead getWord64be
          if e == 0
            then void getWord64be
            else enableLEB128 -- > 9.0
        _            -> return ()

    -- ghc version
    version <- getString
    traceGet ("Version: " ++ version)

    -- way
    way <- getString
    traceGet ("Ways: " ++ show way)

    -- extensible fields (GHC > 9.0)
    when (version >= "9001") $ void getPtr

    -- dict_ptr
    dictPtr <- getPtr
    traceGet ("Dict ptr: " ++ show dictPtr)

    -- dict
    dict <- lookAhead $ getDictionary $ fromIntegral dictPtr

    -- symtable_ptr
    void getPtr
    let versions =
            [ ("8101", getInterface8101)
            , ("8061", getInterface861)
            , ("8041", getInterface841)
            , ("8021", getInterface821)
            , ("8001", getInterface801)
            , ("7081", getInterface781)
            , ("7061", getInterface761)
            , ("7041", getInterface741)
            , ("7021", getInterface721)
            ]
    case snd <$> find ((version >=) . fst) versions of
        Just f  -> f dict
        Nothing -> fail $ "Unsupported version: " <> version


fromFile :: FilePath -> IO (Either String Interface)
fromFile fp = withBinaryFile fp ReadMode go
  where
    go h =
      let feed (G.Done _ _ iface) = pure $ Right iface
          feed (G.Fail _ _ msg) = pure $ Left msg
          feed (G.Partial k) = do
            chunk <- hGetSome h defaultChunkSize
            feed $ k $ if B.null chunk then Nothing else Just chunk
      in feed $ runGetIncremental getInterface 


getULEB128 :: forall a. (Integral a, FiniteBits a) => Get a
getULEB128 =
    go 0 0
  where
    go :: Int -> a -> Get a
    go shift w = do
        b <- getWord8
        let !hasMore = testBit b 7
        let !val = w .|. (clearBit (fromIntegral b) 7 `unsafeShiftL` shift) :: a
        if hasMore
            then do
                go (shift+7) val
            else
                return $! val

getSLEB128 :: forall a. (Integral a, FiniteBits a) => Get a
getSLEB128 = do
    (val,shift,signed) <- go 0 0
    if signed && (shift < finiteBitSize val )
        then return $! ((complement 0 `unsafeShiftL` shift) .|. val)
        else return val
    where
        go :: Int -> a -> Get (a,Int,Bool)
        go shift val = do
            byte <- getWord8
            let !byteVal = fromIntegral (clearBit byte 7) :: a
            let !val' = val .|. (byteVal `unsafeShiftL` shift)
            let !more = testBit byte 7
            let !shift' = shift+7
            if more
                then go shift' val'
                else do
                    let !signed = testBit byte 6
                    return (val',shift',signed)

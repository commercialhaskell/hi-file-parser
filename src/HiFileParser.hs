{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module HiFileParser
  ( Interface (..)
  , List (..)
  , Dictionary (..)
  , Module (..)
  , Usage (..)
  , Dependencies (..)
  , getInterface
  , fromFile
  ) where

{- HLINT ignore "Reduce duplication" -}

import           Control.Monad ( replicateM, replicateM_, when )
import           Data.Binary ( Word32, Word64, Word8)
import qualified Data.Binary.Get as G
                   ( Decoder (..), Get, bytesRead, getByteString, getInt64be
                   , getWord32be, getWord64be, getWord8, lookAhead
                   , runGetIncremental, skip
                   )
import           Data.Bool ( bool )
import           Data.ByteString.Lazy.Internal ( defaultChunkSize )
import           Data.Char ( chr )
import           Data.Functor ( ($>), void )
import           Data.Maybe ( catMaybes )
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ( (<>) )
#endif
import qualified Data.Vector as V
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import           GHC.IO.IOMode ( IOMode (..) )
import           Numeric ( showHex )
import           RIO.ByteString as B ( ByteString, hGetSome, null )
import           RIO ( Generic, Int64, NFData )
import           System.IO ( withBinaryFile )
import           Data.Bits
                   ( FiniteBits (..), (.|.), clearBit, complement, testBit
                   , unsafeShiftL
                   )
import           Control.Monad.State
                   ( StateT, evalStateT, get, gets, lift, modify )
import qualified Debug.Trace

newtype IfaceGetState = IfaceGetState
  { useLEB128 :: Bool -- ^ Use LEB128 encoding for numbers
  }

data IfaceVersion
  = V7021
  | V7041
  | V7061
  | V7081
  | V8001
  | V8021
  | V8041
  | V8061
  | V8101
  | V9001
  | V9041
  | V9045
  | V9081
  | V9120
  deriving (Eq, Enum, Ord, Show)
  -- careful, the Ord matters!


type Get a = StateT IfaceGetState G.Get a

enableDebug :: Bool
enableDebug = False

traceGet :: String -> Get ()
traceGet s
  | enableDebug = Debug.Trace.trace s (return ())
  | otherwise   = return ()

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
  } deriving newtype (NFData, Show)

newtype Dictionary = Dictionary
  { unDictionary :: V.Vector ByteString
  } deriving newtype (NFData, Show)

newtype Module = Module
  { unModule :: ModuleName
  } deriving newtype (NFData, Show)

newtype Usage = Usage
  { unUsage :: FilePath
  } deriving newtype (NFData, Show)

data Dependencies = Dependencies
  { dmods    :: List (ModuleName, IsBoot)
  , dpkgs    :: List (ModuleName, Bool)
  , dorphs   :: List Module
  , dfinsts  :: List Module
  , dplugins :: List ModuleName
  } deriving (Generic, Show)

instance NFData Dependencies

data Interface = Interface
  { deps  :: Dependencies
  , usage :: List Usage
  } deriving (Generic, Show)

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

-- | Get a FastString
--
-- FastStrings are stored in a global FastString table and only the index (a
-- Word32be) is stored at the expected position.
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

getInterfaceRecent :: IfaceVersion -> Dictionary -> Get Interface
getInterfaceRecent version d = do
  void $ traceShow "Module:" getModule
  void $ traceShow "Sig:" $ getMaybe getModule
  void getWord8 -- hsc_src
  getFP         -- iface_hash
  getFP         -- mod_hash
  getFP         -- flag_hash
  getFP         -- opt_hash
  getFP         -- hpc_hash
  getFP         -- plugin_hash
  void getBool  -- orphan
  void getBool  -- hasFamInsts
  ddeps  <- traceShow "Dependencies:" getDependencies
  dusage <- traceShow "Usage:"        getUsage
  pure (Interface ddeps dusage)
 where
  since v = when (version >= v)

  getFastString = getCachedBS d

  getModule = do
    idType <- traceShow "Unit type:" getWord8
    case idType of
      0 -> void getFastString
      1 ->
           void $
           getFastString *> getList (getTuple getFastString getModule)
      _ -> fail $ "Invalid unit type: " <> show idType
    Module <$> getFastString
  getDependencies =
    withBlockPrefix $ do
      if version >= V9041
        then do
          -- warning: transitive dependencies are no longer stored,
          -- only direct imports!
          -- Modules are now prefixed with their UnitId (should have been
          -- ModuleWithIsBoot...)
          direct_mods <- traceShow "direct_mods:" $
            getList (getFastString *> getTuple getFastString getBool)
          direct_pkgs <- getList getFastString

          -- plugin packages are now stored separately
          plugin_pkgs <- getList getFastString
          let all_pkgs = unList plugin_pkgs ++ unList direct_pkgs

          -- instead of a trust bool for each unit, we have an additional
          -- list of trusted units (transitive)
          trusted_pkgs <- getList getFastString
          let trusted u = u `elem` unList trusted_pkgs
              all_pkgs_trust = List (zip all_pkgs (map trusted all_pkgs))

          -- these are new
          _sig_mods  <- getList getModule
          _boot_mods <- getList (getFastString *> getTuple getFastString getBool)

          dep_orphs  <- getList getModule
          dep_finsts <- getList getModule

          -- plugin names are no longer stored here
          let dep_plgins = List []

          pure (Dependencies direct_mods all_pkgs_trust dep_orphs dep_finsts dep_plgins)
        else do
          dep_mods   <- getList (getTuple getFastString getBool)
          dep_pkgs   <- getList (getTuple getFastString getBool)
          dep_orphs  <- getList getModule
          dep_finsts <- getList getModule
          dep_plgins <- getList getFastString
          pure (Dependencies dep_mods dep_pkgs dep_orphs dep_finsts dep_plgins)

  getUsage = withBlockPrefix $ List . catMaybes . unList <$> getList go
   where
    -- this must follow the `Binary Usage` instance in GHC
    -- (in GHC.Unit.Module.Deps, at least in GHC 9.4.5)
    go :: Get (Maybe Usage)
    go = do
      usageType <- traceShow "Usage type:" getWord8
      case usageType of
        0 -> do
          void (traceShow "Module:" getModule) -- usg_mod
          void getFP                           -- usg_mod_hash
          void getBool                         -- usg_safe
          pure Nothing

        1 -> do
          void (traceShow "Home module:" getFastString)   -- usg_mod_name
          since V9045 $ void getFastString                -- usg_unit_id
          void getFP                                      -- usg_mod_hash
          void (getMaybe getFP)                           -- usg_exports
          void getEntitiesList                            -- usg_entities
          void getBool                                    -- usg_safe
          pure Nothing

        2 -> do
          -- usg_file_path
          file_path  <- traceShow "File:" $ if version >= V9081
            then Text.unpack . Text.decodeUtf8 <$> getFastString
            else getString
          void $ traceShow "FP:" getFP'                     -- usg_file_hash
          since V9041 $ void $ traceShow "File label:" (getMaybe getString)-- usg_file_label
          pure (Just (Usage file_path))

        3 -> do
          void getModule -- usg_mod
          void getFP     -- usg_mod_hash
          pure Nothing

        4 | version >= V9041 -> do -- UsageHomeModuleInterface
          void getFastString                  -- usg_mod_name
          since V9045 $ void getFastString    -- usg_unit_id
          void getFP                          -- usg_iface_hash
          pure Nothing

        _ -> fail $ "Invalid usageType: " <> show usageType

  getEntitiesList :: Get (List (ByteString, ()))
  getEntitiesList = getList (getTuple (getNameSpace *> getFastString) getFP)

  -- See `instance Binary NameSpace` in module GHC.Types.Name.Occurrence. We
  -- discard the information.
  getNameSpace :: Get ()
  getNameSpace = if version >= V9081
    then do
      nameSpaceType <- getWord8
      case nameSpaceType of
        0 -> pure ()
        1 -> pure ()
        2 -> pure ()
        3 -> pure ()
        -- Unlike the original, we test that the byte we have obtained is
        -- valid.
        4 -> do
          void getFastString
        _ -> fail $ "Invalid NameSpace type: " <> show nameSpaceType
    else void getWord8

getInterface :: Get Interface
getInterface = do
  let enableLEB128 = modify (\c -> c { useLEB128 = True})
      -- read a relative bin pointer
      getRelPtr = do
        c <- bytesRead
        p <- getPtr
        pure (fromIntegral c + p)

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

  let !ifaceVersion
        | version >= "9120" = V9120
        | version >= "9081" = V9081
        | version >= "9045" = V9045
        | version >= "9041" = V9041
        | version >= "9001" = V9001
        | version >= "8101" = V8101
        | version >= "8061" = V8061
        | version >= "8041" = V8041
        | version >= "8021" = V8021
        | version >= "8001" = V8001
        | version >= "7081" = V7081
        | version >= "7061" = V7061
        | version >= "7041" = V7041
        | version >= "7021" = V7021
        | otherwise         = error $ "Unsupported version: " <> version

  -- way
  way <- getString
  traceGet ("Ways: " ++ show way)

  -- source hash (GHC >= 9.4)
  when (ifaceVersion >= V9041) $ void getFP

  -- extensible fields (GHC >= 9.0)
  when (ifaceVersion >= V9001) $ void getPtr

  -- dict_ptr
  dictPtr <- if ifaceVersion >= V9120 -- 9.12 uses relative pointers
    then getRelPtr
    else getPtr
  traceGet ("Dict ptr: " ++ show dictPtr)

  -- dict
  dict <- lookAhead $ getDictionary $ fromIntegral dictPtr

  -- symtable_ptr
  void getPtr

  -- IfaceType table
  when (ifaceVersion >= V9120) $ void getPtr

  case ifaceVersion of
    V9120 -> getInterfaceRecent ifaceVersion dict
    V9081 -> getInterfaceRecent ifaceVersion dict
    V9045 -> getInterfaceRecent ifaceVersion dict
    V9041 -> getInterfaceRecent ifaceVersion dict
    V9001 -> getInterfaceRecent ifaceVersion dict
    V8101 -> getInterfaceRecent ifaceVersion dict
    V8061 -> getInterface861 dict
    V8041 -> getInterface841 dict
    V8021 -> getInterface821 dict
    V8001 -> getInterface801 dict
    V7081 -> getInterface781 dict
    V7061 -> getInterface761 dict
    V7041 -> getInterface741 dict
    V7021 -> getInterface721 dict

fromFile :: FilePath -> IO (Either String Interface)
fromFile fp = withBinaryFile fp ReadMode go
 where
  go h =
    let feed (G.Done _ _ iface) = pure $ Right iface
        feed (G.Fail _ _ msg) = pure $ Left msg
        feed (G.Partial k) = do
          chunk <- hGetSome h defaultChunkSize
          feed $ k $ if B.null chunk then Nothing else Just chunk
    in  feed $ runGetIncremental getInterface


getULEB128 :: forall a. (Integral a, FiniteBits a) => Get a
getULEB128 =
  go 0 0
 where
  go :: Int -> a -> Get a
  go shift w = do
    b <- getWord8
    let !hasMore = testBit b 7
        !val = w .|. (clearBit (fromIntegral b) 7 `unsafeShiftL` shift) :: a
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
        !val' = val .|. (byteVal `unsafeShiftL` shift)
        !more = testBit byte 7
        !shift' = shift+7
    if more
      then go shift' val'
      else do
        let !signed = testBit byte 6
        return (val',shift',signed)

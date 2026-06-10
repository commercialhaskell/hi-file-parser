{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module HiFileParserSpec (spec) where

import qualified Data.List             as List
import qualified HiFileParser          as Iface
import           RIO
import           Test.Hspec            (Spec, describe, it, shouldBe)

type Version = String
type Directory = FilePath
type Usage = String
type Module = ByteString

-- | GHC x.y.z is represented as \"ghcxyyz\" where yy is padded with zeros.
versions32 :: [Version]
versions32 =
  [ "ghc7103"  -- Last in GHC 7.10 series, using GHC 7.8.1 format
  , "ghc8002"  -- Last in GHC 8.0 series, using GHC 8.0.1 format
  , "ghc8022"  -- Last in GHC 8.2 series, using GHC 8.2.1 format
  , "ghc8044"  -- Last in GHC 8.4 series, using GHC 8.4.1 format
  ]

-- | GHC x.y.z is represented as \"ghcxyyz\" where yy is padded with zeros.
versions64 :: [Version]
versions64 =
  [ "ghc8022"  -- Last in GHC 8.2 series, using GHC 8.0.1 format
  , "ghc8044"  -- Last in GHC 8.4 series, using GHC 8.4.1 format
  , "ghc8065"  -- Last in GHC 8.6 series, using GHC 8.6.1 format
  , "ghc8084"  -- Last in GHC 8.8 series, using GHC 8.6.1 format
  , "ghc8107"  -- Last in GHC 8.10 series, using GHC 8.10.1 format
  , "ghc9002"  -- Last in GHC 9.0 series, using GHC 9.0.1 format
  , "ghc9028"  -- Last in GHC 9.2 series, using GHC 9.0.1 format
  , "ghc9044"  -- Last using GHC 9.4.1 format
  , "ghc9048"  -- Last in GHC 9.4 series, using GHC 9.4.5 format
  , "ghc9067"  -- Last in GHC 9.6 series, using GHC 9.4.5 format
  , "ghc9084"  -- Last in GHC 9.8 series, using GHC 9.8.1 format
  , "ghc9102"  -- Last in GHC 9.10 series, using GHC 9.8.1 format
  , "ghc9122"  -- Last in GHC 9.12 series, using GHC 9.12 format
  , "ghc9140"  -- First in GHC 9.14 series, using GHC 9.14 format
  ]

-- | Interface files that exercise two distinct bugs in
-- @getInterfaceRecent@:
--
-- (A) @getModule@ recognises only unit tags @0@ (@RealUnit@) and @1@
--     (@VirtUnit@). GHC's @Binary Unit@ instance also writes byte @2@
--     for @HoleUnit@ (the sentinel for an un-filled signature hole).
--     @LogHelper.hi@'s own module reference is a @VirtUnit@ whose
--     instantiation-substitution table maps each hole name to a @Module@
--     with @HoleUnit@ as its unit, so the parser bails at byte @2@
--     before it ever reaches the dependency section.
--
-- (B) The parser reads @dep_sig_mods@ as @skipList getModule@, but GHC
--     declares the field as @dep_sig_mods :: ![ModuleName]@ and
--     serialises it as a list of cached @FastString@s (one byte per
--     element on the wire in the small-index case). For an empty
--     @dep_sig_mods@ this happens to work; for any non-empty value the
--     parser walks off into the wrong part of the byte stream and trips
--     on whatever byte happens to land at the next \"unit tag\" read.
--     @Consumer.hi@ and @Main.hi@ have non-empty @dep_sig_mods@ and
--     both trip on byte @4@.
--
-- Provenance: these fixtures were produced by @stack build@ in
-- commercialhaskell/stack PR #6865 (GHC 9.10.3). That PR's cross-package
-- support work is the most common way to make GHC write the byte
-- patterns above in practice, but neither parser bug is specific to it.
signatureFiles :: [FilePath]
signatureFiles =
  [ "LogHelper.hi"  -- indefinite library: HoleUnit reference (tag 2)
  , "Consumer.hi"   -- instantiated library (tag 4)
  , "Main.hi"       -- instantiated executable Main (tag 4)
  ]

spec :: Spec
spec = do
  describe "should successfully deserialize interface for" $ do
    traverse_ (deserialize check32 . ("x32/" <>)) versions32
    traverse_ (deserialize check64 . ("x64/" <>)) versions64
  describe "signature-bearing interfaces" $ do
    describe "Bug A: getModule should recognise HoleUnit (tag 2)" $
      -- Fails today with @Left "Invalid unit type: 2"@. Becomes green as
      -- soon as @getModule@ handles tag @2@ as @HoleUnit@ with no payload,
      -- independently of Bug B. After Bug A is fixed the file still fails
      -- to parse fully because of Bug B (different error: \"Invalid unit
      -- type: 6\"), but this targeted assertion is already satisfied.
      --
      -- ---- GHC source references ----
      -- Mechanism: @HoleUnit@ constructor with @putByte bh 2@ in
      -- @instance Binary Unit@. Introduced by Sylvain Henry on 2020-04-03
      -- in commit @10d15f1ec4ba@ ("Refactoring unit management code";
      -- commit message: "Replace BackPack fake 'hole' UnitId by a proper
      -- HoleUnit constructor."):
      --   https://github.com/ghc/ghc/commit/10d15f1ec4bab4dd6152d87fc66e61658a705eb3
      --
      -- First GHC release containing it: GHC 9.0.1, tag
      -- @ghc-9.0.1-release@ (tag object @da53a348@, tagged 2021-02-04).
      -- At that release the file had been renamed to
      -- @compiler/GHC/Unit/Types.hs@; the relevant lines are:
      --   * @| HoleUnit@ constructor, line 247:
      --     https://github.com/ghc/ghc/blob/da53a348150d30193a6f28e1b7ddcabdf45ab726/compiler/GHC/Unit/Types.hs#L247
      --   * @put_ bh HoleUnit = do; putByte bh 2@, lines 382-383:
      --     https://github.com/ghc/ghc/blob/da53a348150d30193a6f28e1b7ddcabdf45ab726/compiler/GHC/Unit/Types.hs#L382-L383
      --
      -- Same byte-2 encoding has been carried forward unchanged in 9.2,
      -- 9.4, 9.6, 9.8, 9.10, 9.12, 9.14 - so this bug applies to every
      -- GHC release from 9.0.1 onward.
      it "x64/ghc9103-signatures/LogHelper.hi: parser must not bail at the HoleUnit byte" $
        parseShouldNotBailWith "Invalid unit type: 2" $
          "test-files/iface/x64/ghc9103-signatures/LogHelper.hi"
    describe "Bug B: dep_sig_mods is [ModuleName], not [Module]" $ do
      -- These two fixtures fail today with @Left "Invalid unit type: 4"@.
      -- They become green as soon as @dep_sig_mods@ is read as a list of
      -- cached FastStrings, independently of Bug A. (Their own module
      -- references are @RealUnit@, so they never touch the HoleUnit path
      -- - Bug A's fix is neither necessary nor sufficient.)
      --
      -- ---- GHC source references ----
      -- Mechanism: @dep_sig_mods :: ![ModuleName]@ field on
      -- @Dependencies@, written by @instance Binary Dependencies@.
      -- Introduced by Matthew Pickering on 2021-05-05 in commit
      -- @38faeea1a940@ ("Remove transitive information about modules and
      -- packages from interface files", GHC issue #16885):
      --   https://github.com/ghc/ghc/commit/38faeea1a94072ffd9f459d9fe570f06bc1da84a
      --
      -- The GHC 9.2 release branch was forked before this commit landed
      -- (merge-base of the commit and the @ghc-9.2.1-release@ tag is
      -- @05c5c054@, 2021-03-15), so GHC 9.2.x shipped without the field.
      -- First GHC release containing it: GHC 9.4.1, tag
      -- @ghc-9.4.1-release@ (tag object @c24e9dc5@, tagged 2022-08-07).
      -- At that release, in @compiler/GHC/Unit/Module/Deps.hs@:
      --   * @, dep_sig_mods :: ![ModuleName]@, line 62:
      --     https://github.com/ghc/ghc/blob/6d01245c458c49ca25c89ec13be3268ab6930a27/compiler/GHC/Unit/Module/Deps.hs#L62
      --   * @put_ bh (dep_sig_mods deps)@ (inside the @instance Binary
      --     Dependencies@ block at lines 111-119), line 115:
      --     https://github.com/ghc/ghc/blob/6d01245c458c49ca25c89ec13be3268ab6930a27/compiler/GHC/Unit/Module/Deps.hs#L115
      --
      -- The field has been carried forward unchanged in 9.6, 9.8, 9.10,
      -- 9.12, 9.14 - so this bug applies to every GHC release from 9.4.1
      -- onward.
      it "x64/ghc9103-signatures/Consumer.hi: parser must not misalign on dep_sig_mods" $
        parseShouldNotBailWith "Invalid unit type: 4" $
          "test-files/iface/x64/ghc9103-signatures/Consumer.hi"
      it "x64/ghc9103-signatures/Main.hi: parser must not misalign on dep_sig_mods" $
        parseShouldNotBailWith "Invalid unit type: 4" $
          "test-files/iface/x64/ghc9103-signatures/Main.hi"
    describe "Bug A + Bug B: should fully deserialize" $
      -- Stricter than the per-bug assertions above: requires both fixes
      -- to be in place. Useful as the regression gate once the bugs are
      -- closed.
      traverse_ (deserializeFile "x64/ghc9103-signatures") signatureFiles

check32 :: Iface.Interface -> IO ()
check32 iface = do
    hasExpectedUsage "some-dependency.txt" iface `shouldBe` True

check64 :: Iface.Interface -> IO ()
check64 iface = do
    hasExpectedUsage "Test.h" iface `shouldBe` True
    hasExpectedUsage "README.md" iface `shouldBe` True
    hasExpectedModule "X" iface `shouldBe` True

deserialize :: (Iface.Interface -> IO ()) -> Directory -> Spec
deserialize check d = do
    it d $ do
        let ifacePath = "test-files/iface/" <> d <> "/Main.hi"
        result <- Iface.fromFile ifacePath
        case result of
          (Left msg)    -> fail msg
          (Right iface) -> check iface

deserializeFile :: Directory -> FilePath -> Spec
deserializeFile d fname =
    it (d <> "/" <> fname) $ do
        let ifacePath = "test-files/iface/" <> d <> "/" <> fname
        result <- Iface.fromFile ifacePath
        case result of
          Left msg -> fail msg
          Right _  -> pure ()

-- | Assert that @Iface.fromFile@ either succeeds or fails with a message that
-- does not contain the given substring. Lets a per-bug failing test pass as
-- soon as that bug alone is fixed, even if other unrelated bugs leave the
-- parser still erroring on the same fixture with a different message.
parseShouldNotBailWith :: String -> FilePath -> IO ()
parseShouldNotBailWith forbidden fp = do
    result <- Iface.fromFile fp
    case result of
      Left msg | forbidden `List.isInfixOf` msg ->
        fail $ "parser bailed with " <> show forbidden <> ": " <> msg
      _ -> pure ()

-- | `Usage` is the name given by GHC to TH dependency
hasExpectedUsage :: Usage -> Iface.Interface -> Bool
hasExpectedUsage u =
    elem u . fmap Iface.unUsage . Iface.unList . Iface.usage

hasExpectedModule :: Module -> Iface.Interface -> Bool
hasExpectedModule m =
    elem m . fmap fst . Iface.unList . Iface.dmods . Iface.deps

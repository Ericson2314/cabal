{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}   -- for importing "Distribution.Compat.Prelude.Internal"

module UnitTests.Distribution.Types.GenericPackageDescription where

import Prelude ()
import Distribution.Compat.Lens
import Distribution.Compat.Prelude.Internal


import Distribution.Types.Condition
import Distribution.Types.CondTree

import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription

import Distribution.Types.Library
import Distribution.Types.Executable
import Distribution.Types.Benchmark
import Distribution.Types.ForeignLib
import Distribution.Types.TestSuite

import Distribution.Types.BuildInfo
import Distribution.Types.Dependency

import Distribution.Types.BenchmarkInterface
import Distribution.Types.ExecutableScope
--import Distribution.Types.ForeignLibOption
import Distribution.Types.ForeignLibType
import Distribution.Types.TestSuiteInterface
import Distribution.Types.PackageName
import Distribution.Types.UnqualComponentName


import qualified Distribution.Types.BuildInfo.Lens as L
--import qualified Distribution.Types.GenericPackageDescription.Lens as L

import UnitTests.Distribution.Version ()

import qualified Control.Exception as C
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testCase "GenericPackageDescription deepseq" gpdDeepseq
  , testGroup "GenericPackageDescription lenses" gpdLenses
  ]

gpdFields :: [(String, GenericPackageDescription -> GenericPackageDescription)]
gpdFields =
  [ ("packageDescription", \gpd -> gpd { packageDescription = undefined })
  , ("genPackageFlags",    \gpd -> gpd { genPackageFlags    = undefined })
  , ("condLibrary",        \gpd -> gpd { condLibrary        = undefined })
  , ("condSubLibraries",   \gpd -> gpd { condSubLibraries   = undefined })
  , ("condForeignLibs",    \gpd -> gpd { condForeignLibs    = undefined })
  , ("condExecutables",    \gpd -> gpd { condExecutables    = undefined })
  , ("condTestSuites",     \gpd -> gpd { condTestSuites     = undefined })
  , ("condBenchmarks",     \gpd -> gpd { condBenchmarks     = undefined })
  ]

gpdDeepseq :: Assertion
gpdDeepseq = sequence_
  [ throwsUndefined msg (f emptyGenericPackageDescription) | (msg, f) <- gpdFields ]

throwsUndefined :: NFData a => String -> a -> Assertion
throwsUndefined field a =
  C.catch (C.evaluate (rnf a) >> assertFailure ("Deepseq failed to evaluate " ++ show field))
          (\(C.ErrorCall _) -> return ())

gpdLenses :: [TestTree]
gpdLenses =
  [ testProperty "same deps" sameDep

  ]

sameDep :: GenericPackageDescription -> Dependency -> Bool
sameDep gpd dep = all (== dep) deps
  where
    depLens :: Traversal' GenericPackageDescription Dependency
    depLens = L.traverseBuildInfos . L.targetBuildDepends . traverse
    gpd' :: GenericPackageDescription
    gpd' = set depLens dep gpd
    deps :: [Dependency]
    deps = toListOf depLens gpd'

instance Arbitrary GenericPackageDescription where
  arbitrary = GenericPackageDescription
    <$> pure emptyPackageDescription
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Flag where
  arbitrary = MkFlag
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary FlagName where

  arbitrary = mkFlagName <$> arbitraryIdent

instance Arbitrary ConfVar where
  arbitrary = oneof [ Flag <$> arbitrary ] -- TODO

instance (Arbitrary v, Arbitrary c, Arbitrary a) => Arbitrary (CondTree v c a) where
  arbitrary = CondNode <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary v, Arbitrary c, Arbitrary a) => Arbitrary (CondBranch v c a) where
  arbitrary = CondBranch <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary c => Arbitrary (Condition c) where
  arbitrary = oneof [ Var <$> arbitrary
                    , Lit <$> arbitrary
                    , CNot <$> arbitrary
                    , COr <$> arbitrary <*> arbitrary
                    , CAnd <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary Library where
  arbitrary = Library
    <$> arbitrary
    <*> pure []
    <*> pure []
    <*> pure []
    <*> pure True
    <*> arbitrary

instance Arbitrary Executable where
  arbitrary = Executable
    <$> arbitrary
    <*> pure ""
    <*> pure ExecutablePublic
    <*> arbitrary

instance Arbitrary Benchmark where
  arbitrary = Benchmark
    <$> arbitrary
    <*> (BenchmarkExeV10 <$> arbitrary <*> pure "")
    <*> arbitrary

instance Arbitrary ForeignLib where
  arbitrary = ForeignLib
    <$> arbitrary
    <*> pure ForeignLibNativeShared
    <*> pure []
    <*> arbitrary
    <*> pure Nothing
    <*> pure Nothing
    <*> pure []

instance Arbitrary TestSuite where
  arbitrary = TestSuite
    <$> arbitrary
    <*> (TestSuiteExeV10 <$> arbitrary <*> pure "")
    <*> arbitrary

instance Arbitrary BuildInfo where
  arbitrary = L.targetBuildDepends (const arbitrary) mempty

instance Arbitrary Dependency where
  arbitrary = Dependency <$> arbitrary <*> arbitrary

instance Arbitrary PackageName where
  arbitrary = mkPackageName <$> arbitrary

instance Arbitrary UnqualComponentName where
  arbitrary = mkUnqualComponentName <$> arbitraryIdent

arbitraryIdent :: Gen String
arbitraryIdent = listOf $ elements ['a'..'z']

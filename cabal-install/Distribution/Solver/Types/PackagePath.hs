{-# LANGUAGE DeriveGeneric #-}
module Distribution.Solver.Types.PackagePath
    ( PackagePath(..)
    , Namespace(..)
    , Qualifier
    , QualifierEdge(..)
    , QualExeTarget(..)
    , dispQualifier
    , dispQualifierEdge
    , Qualified(..)
    , QPN
    , dispQPN
    , showQPN
    ) where

import Data.List (foldl')

import Distribution.Package
import Distribution.Text
import qualified Text.PrettyPrint as Disp
import Distribution.Client.Compat.Prelude (Binary, Generic, (<<>>))

-- | A package path consists of a namespace and a package path inside that
-- namespace.
data PackagePath = PackagePath Namespace Qualifier
  deriving (Eq, Ord, Show)

-- | Top-level namespace
--
-- Package choices in different namespaces are considered completely independent
-- by the solver.
data Namespace =
    -- | The default namespace
    DefaultNamespace

    -- | Independent namespace
    --
    -- For now we just number these (rather than giving them more structure).
  | Independent Int
  deriving (Eq, Ord, Show)

-- | Pretty-prints a namespace. The result is either empty or
-- ends in a period, so it can be prepended onto a qualifier.
dispNamespace :: Namespace -> Disp.Doc
dispNamespace DefaultNamespace = Disp.empty
dispNamespace (Independent i) = Disp.int i <<>> Disp.text "."

-- | Qualifier of a package within a namespace (see 'PackagePath')
--
-- A qualifier represents a path between nearly disconnected components in the
-- dependency graph. For example a top-level dependency in this namespace would
-- be the empty list
--
-- While in theory the path should reach all the way to the nearly-disconnected
-- component, in practice we may prune it so that the solver will terminate, or
-- terminate in a reasonable time.
--
-- Note that for efficiency reasons, the path is store **reversed** vs the order
-- it is displayed/parse/generally thought of: i.e. it is stored from deepest
-- edge to shallowest edge.
type Qualifier = [QualifierEdge]

-- | A "Qualifying" dependency graph edge.
--
-- Dependency edges are deemed so if they strongly encapsulate the deps they
-- transitively point to. This normally comes from exe deps, where any library
-- choices for the exe do not impact downstream using that exe at all
-- (e.g. there is no types that need to be compatible, exe interfaces are
-- informal unlike Haskell ones).
--
-- Note that while qualifiers are stored least-specific to most-specific, they
-- are rendered and parsed the other way around. Maybe we should use more types
-- to make this clearer?
data QualifierEdge

    -- | Any dependency on base is considered independent
    --
    -- This makes it possible to have base shims. This is kind of a hack as it
    -- only applies to immediate dependencies named base, not the transitive
    -- dependencies like a proper exe dependency edge.
  = QualBase PackageName

    -- | If we depend on an executable from a package (via @build-tools@,
    -- @build-tool-depends@ or the implicit dependency on the setup executable),
    -- we should solve for the dependencies of that executable separately (since
    -- we're not going to actually try to link it.)
    --
    -- A dependency edge, being part of a directed graph, naturally connects two
    -- nodes, the source and the destiantion; we went to keep track of both with
    -- @'QaulExe' pn1 pn2@. If we tracked only @pn1@, that would require a
    -- consistent dependency resolution for all of the depended upon executables
    -- from a package; if we tracked only @pn2@, that would require us to pick
    -- only one version of an executable over the entire install plan.)
    --
    -- The package target may be @TargetOwnSetup@ in addition to another package
    -- because of the aforementioned implicit dependency of a component on a
    -- setup executable from its own package. In a future where we solver
    -- per-component, this special case will fall out naturally.
  | QualExe PackageName QualExeTarget
  deriving (Eq, Ord, Show)

-- | @Maybe PackageName@-isomorphic type used for for clarity. See the @QualExe@
-- variant in @QualifierEdge@.
data QualExeTarget = TargetExe PackageName | TargetOwnSetup
  deriving (Eq, Ord, Show, Generic)

instance Binary QualExeTarget

-- | Pretty-prints a qualifier. The result is either empty or
-- ends in a period, so it can be prepended onto a package name.
--
-- NOTE: the base qualifier is for a dependency _on_ base; the qualifier is
-- there to make sure different dependencies on base are all independent.
-- So we want to print something like @"A.base"@, where the @"A."@ part
-- is the qualifier and @"base"@ is the actual dependency (which, for the
-- 'Base' qualifier, will always be @base@).
dispQualifier :: Qualifier -> Disp.Doc
-- The left fold does the reverse to render for us.
dispQualifier = foldl' (<<>>) Disp.empty . fmap encloseDot where
  encloseDot cur = Disp.text "(" <<>> dispQualifierEdge cur <<>> Disp.text ")."

-- | Pretty-prints a qualifier edge.
--
-- NOTE: the base qualifier is for a dependency _on_ base; the qualifier is
-- there to make sure different dependencies on base are all independent.
-- So we want to print something like @"A.base"@, where the @"A."@ part
-- is the qualifier and @"base"@ is the actual dependency (which, for the
-- 'Base' qualifier, will always be @base@).
dispQualifierEdge :: QualifierEdge -> Disp.Doc
dispQualifierEdge (QualExe pn TargetOwnSetup) =
  disp pn <<>> Disp.text "->" <<>> disp pn <<>> Disp.text ":setup"
dispQualifierEdge (QualExe pn (TargetExe pn2)) =
  -- We have the information in the role to list the exact exe here, but this is
  -- disingenuous until we have per-component solving
  disp pn <<>> Disp.text "->" <<>> disp pn2 <<>> Disp.text ":*"
dispQualifierEdge (QualBase pn) =
  Disp.text "base->" <<>> disp pn <<>> Disp.text ":*"

-- | A qualified entity. Pairs a package path with the entity.
data Qualified a = Q PackagePath a
  deriving (Eq, Ord, Show)

-- | Qualified package name.
type QPN = Qualified PackageName

-- | Pretty-prints a qualified package name.
dispQPN :: QPN -> Disp.Doc
dispQPN (Q (PackagePath ns qual) pn) =
  dispNamespace ns <<>> dispQualifier qual <<>> disp pn

-- | String representation of a qualified package name.
showQPN :: QPN -> String
showQPN = Disp.renderStyle flatStyle . dispQPN

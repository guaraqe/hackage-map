import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

import System.FilePath
import System.Directory.Tree

import Data.GraphViz
import Data.GraphViz.Algorithms
import Data.GraphViz.Attributes.Complete hiding (Dir)
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.Hashable (hash)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

import Control.Monad (forM)
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

hackagePath = "../hackage/"

main = do
  (_ :/ (Dir _ hackage)) <- readDirectoryWith (const (return ())) hackagePath
  let latest = mapMaybe (latestPackage hackagePath) hackage
  deps <- forM latest $ \(package, cabal) -> do
    desc <- readPackageDescription silent cabal
    return (package, packageDependencies desc)

  let graph = transitiveReduction $ graphToDot graphvizParams (hackageGraph deps)

  runGraphvizCommand Sfdp graph Pdf "graph.pdf"

--------------------------------------------------------------------------------
-- Get dependencies from .cabal file

packageDependencies :: GenericPackageDescription -> [String]
packageDependencies desc = fmap unPackageName $
  (concatMap dependencies . toList . condLibrary) desc <>
  (concatMap (dependencies . snd) . condExecutables) desc

dependencies :: CondTree a [Dependency] b -> [PackageName]
dependencies (CondNode _ deps _) =
  fmap (\(Dependency dep _) -> dep) deps

--------------------------------------------------------------------------------
-- Get latest version from folder

latestPackage :: FilePath -> DirTree () -> Maybe (String, FilePath)
latestPackage _ (Failed _ _) = Nothing
latestPackage _ (File _ _) = Nothing
latestPackage hackage (Dir name dirs) =
  Just (name, hackage </> name </> getLatest dirs </> name <.> "cabal")

getLatest :: [DirTree ()] -> String
getLatest = maximum . foldr dirName []
  where
    dirName (Failed _ _) l = l
    dirName (File _ _) l = l
    dirName (Dir name _) l = name : l

--------------------------------------------------------------------------------
-- Create Hackage graph

type HNode = LNode String
type HEdge = LEdge String
type HGraph = Gr String String

hackageGraph :: [(String, [String])] -> HGraph
hackageGraph l =
  mkGraph
    (makeNode . fst <$> l)
    (makeEdge =<< l)

makeNode :: String -> HNode
makeNode name = (hash name, name)

makeEdge :: (String, [String]) -> [HEdge]
makeEdge (name, deps) =
  let hname = hash name
  in fmap (\d -> (hname, hash d, "")) deps

--------------------------------------------------------------------------------
-- Grapviz params

graphvizParams :: GraphvizParams n String String () String
graphvizParams = nonClusteredParams {
  globalAttributes =
    [ GraphAttrs
      [ Overlap (PrismOverlap Nothing)
      , OutputOrder EdgesFirst
      ]
    , NodeAttrs
       [ Style
         [ SItem Filled []
         ]
       ]
    ],
  fmtNode = \(_,label) -> [Label . StrLabel . Text.pack $ label],
  fmtEdge = \(_,_,label) -> [Label . StrLabel . Text.pack $ label]
  }

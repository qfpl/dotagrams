module Data.Propagator.Diagram where

import Control.Monad ()
import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT), ask, reader)
import Data.GraphViz.Attributes (Attribute, Labellable, Shape (Square), X11Color (Transparent), toLabel, shape, style, invis, bgColor)
import Data.GraphViz.Attributes.Complete (Attribute (RankDir), RankDir (FromLeft))
import Data.GraphViz.Types (GraphID (Str), printDotGraph)
import Data.GraphViz.Types.Monadic (Dot, DotM, digraph, graphAttrs, node)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.Text.Lazy
import Data.Foldable (traverse_)
import Numeric.Natural (Natural)

type RevealM s a = ReaderT Natural (DotM s) a
type Reveal s = RevealM s ()

runReveal :: RevealM s a -> Natural -> DotM s a
runReveal r n = l2r *> runReaderT r n
  where l2r = graphAttrs [RankDir FromLeft]

always :: DotM s a -> RevealM s a
always = ReaderT . const
always' :: ([Attribute] -> DotM s a) -> RevealM s a
always' ad = always $ ad []
propagator :: Labellable a => s -> a -> [Attribute] -> Dot s
propagator name val extras = node name $ toLabel val : shape Square : extras
cell :: Labellable a => s -> a -> [Attribute] -> Dot s
cell name val extras = node name $ toLabel val : extras

on :: Natural -> ([Attribute] -> DotM s a) -> RevealM s a
on trigger f =
  ReaderT $ \n ->
    if n >= trigger
    then f []
    else f [style invis]

observe :: (Natural -> [Attribute] -> RevealM s a) -> [Attribute] -> RevealM s a
observe nad a = ask >>= flip nad a

--------------------
-- TODO move?
--------------------
dotFile :: FilePath -> DotGraph String -> IO ()
dotFile fn = writeFile fn . unpack . printDotGraph

digraph_ :: String -> DotM n a -> DotGraph n
digraph_ s dotn = digraph (Str (pack s)) $ do
  graphAttrs [bgColor Transparent]
  dotn

{-# language GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}

module Data.Propagator.Diagram where

import Control.Monad (when)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.State.Class (MonadState, get, modify)
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import Control.Monad.Trans.State (StateT, evalStateT, execStateT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (Bifunctor (second))
import Data.GraphViz.Attributes (Attribute, Labellable, Shape (Square), X11Color (Transparent), toLabel, shape, style, invis, bgColor)
import Data.GraphViz.Attributes.Complete (Attribute (RankDir), RankDir (FromLeft))
import Data.GraphViz.Types (GraphID (Str), printDotGraph)
import Data.GraphViz.Types.Monadic (Dot, DotM (DotM), digraph, graphAttrs, node)
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty, sortWith)
import Data.Text.Lazy (pack, unpack)
import Numeric.Natural (Natural)

newtype RevealM' m a = RevealM { runRevealM :: ReaderT Slide (StateT Slide m) a }
  deriving (Functor, Applicative, Monad, MonadReader Slide, MonadState Slide, MonadFix)

instance MonadTrans RevealM' where
  lift = RevealM . lift . lift

type RevealM s = RevealM' (DotM s)
type Reveal s = RevealM s ()
type Slide = Natural

runReveal :: forall s a . Bool -> RevealM s a -> [DotM s a]
runReveal shouldL2R r =
  let
    l2r = graphAttrs [RankDir FromLeft]
    rsa = runRevealM $ when shouldL2R (lift l2r) *> r
    lastSlide :: Slide
    lastSlide = case flip execStateT 0 $ runReaderT rsa 0 of
      DotM (n,_) -> n
    drawSlide :: Natural -> DotM s a
    drawSlide n = flip evalStateT 0 $ runReaderT rsa n
  in
    drawSlide <$> [0..lastSlide]

--------------------
-- Combinators
--------------------
always :: ([Attribute] -> DotM s a) -> RevealM s a
always ad = lift $ ad []

propagator :: Labellable a => s -> a -> [Attribute] -> Dot s
propagator name val extras = node name $ toLabel val : shape Square : extras

cell :: Labellable a => s -> a -> [Attribute] -> Dot s
cell name val extras = node name $ toLabel val : extras

slide :: RevealM s Slide
slide = do
  modify (+1)
  get

currentSlide :: RevealM s Slide
currentSlide = ask

switch :: Slide -> a -> a -> RevealM s a
switch trigger a b = do
  cs <- currentSlide
  pure $ if cs < trigger then a else b

switchVis :: Slide -> RevealM s [Attribute]
switchVis trigger = switch trigger [style invis] []

switchLabel :: Slide -> String -> RevealM s String
switchLabel trigger label = switch trigger (' ' <$ label) label

switchLabels :: NonEmpty (Slide,String) -> RevealM s String
switchLabels events = do
  cs <- currentSlide
  let
    events' = second pad <$> sortWith fst events
    maxLen = maximum . fmap (length . snd) $ events
    space = ' '
    spaces = replicate maxLen space
    pad str =
      let
        len = length str
        diff = maxLen - len
        halfDiff = diff `div` 2
        halfSpaces = replicate halfDiff space
        addExtraSpace = if odd diff then (space:) else id
      in
        addExtraSpace $
          halfSpaces ++ str ++ halfSpaces

    go earlier (trigger,now) = if cs < trigger then earlier else now
  pure $ foldl' go spaces events'

reveal :: Slide -> ([Attribute] -> DotM s a) -> RevealM s a
reveal s f = switchVis s >>= lift . f

--------------------
-- TODO move?
--------------------
dotFile :: FilePath -> DotGraph String -> IO ()
dotFile fn = writeFile fn . unpack . printDotGraph

digraph_ :: String -> DotM n a -> DotGraph n
digraph_ s dotn = digraph (Str (pack s)) $ do
  graphAttrs [bgColor Transparent]
  dotn

module Main where

import Control.Monad.Trans.Reader (ReaderT (ReaderT, runReaderT))
import Data.List (intercalate)
import Data.Propagator.Diagram
import Data.GraphViz.Attributes (style, invis)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised (DotGraph)
import Data.Foldable (for_)
import Data.Traversable (for)
import Numeric.Natural (Natural)
import System.Process (system)

diagram :: Reveal String
diagram = do
  always' $ cell "inL" ""
  always' $ cell "inR" ""
  ReaderT $ \n ->
    propagator "add"
      (if n >= 2 then "+" else "")
      (if n >= 1
        then []
        else [style invis])

  on 1 $ edge "inL" "add"
  on 1 $ edge "inR" "add"

outputs :: [(Natural,DotGraph String)]
outputs =
  map
    (\n -> (n, digraph_ (show n) $ runReveal diagram n))
    [0,1,2]

main :: IO ()
main =
  for_ outputs $ \(i,g) -> do
    let fndot = "example" <> show i <> ".dot"
        fnpdf = "example" <> show i <> ".pdf"
        space = " "
    dotFile fndot g
    system $ intercalate " "
      [ "dot"
      , fndot
      , "-Tpdf"
      , "-o"
      , fnpdf
      ]
    system $ intercalate " "
      [ "firefox"
      , fnpdf
      ]

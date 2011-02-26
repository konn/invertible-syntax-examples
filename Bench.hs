module Main where
import JSONParsec
import JSONPretty
import JSONInvertible
import ParsecInvert
import qualified Text.Syntax.Printer.Naive as PP
import JSON
import JSONTree
import Progression.Main
import qualified Text.Parsec as P
import Criterion.Main hiding (defaultMain)
import Data.Maybe

import Control.DeepSeq
import Control.Parallel.Strategies

import System.Environment

main = do
  (a:as) <- getArgs
{-
  s1000 <- readFile "bench-1000.json"
  src1000 <- return $! s1000 `using` rdeepseq
  s5000 <- readFile "bench-5000.json"
  src5000 <- return $! s5000 `using` rdeepseq
-}
  s100 <- readFile "bench-100.json"
  src100 <- return $! s100 `using` rdeepseq
  s500 <- readFile "bench-500.json"
  src500 <- return $! s500 `using` rdeepseq
  let (p,pp) = case a of
                 "parsec" -> (parseJSON, prettyJSON)
                 "invertible" -> (head . parse json, fromJust . PP.print json)
                 "parsec-invertible" -> (either (error.show) id . P.parse json "", undefined)
  withArgs as $ defaultMain $
    bgroup "bench"
      [ bgroup "parsing"
                   [ bench "100" $ nf p src100
                   , bench "500" $ nf p src500
                   ]
      , bgroup "printing"
                   [ bench "100" $ nf pp js100
                   , bench "500" $ nf pp js500
                   ]
      ]


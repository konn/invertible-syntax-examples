{-# LANGUAGE TypeSynonymInstances #-}
module ParsecInvert () where
import Control.Isomorphism.Partial
import Text.Syntax
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Control.Applicative as A
import Data.Maybe
import Control.Monad

instance IsoFunctor P.Parser where
  iso <$> pa = do
    a <- pa
    maybe empty return (apply iso a)

instance ProductFunctor P.Parser where
  pa <*> pb = (,) A.<$> pa A.<*> pb

instance Alternative P.Parser where
  a <|>b = P.try a A.<|> b
  empty = A.empty

instance Syntax P.Parser where
  pure  = A.pure
  token = P.anyToken

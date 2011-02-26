{-# LANGUAGE TemplateHaskell #-}
module JSON where
import Data.DeriveTH
import Control.DeepSeq

data JSON = Number !Integer
          | Null
          | Bool !Bool
          | Array ![JSON]
          | String String
          | Object ![(String, JSON)]
            deriving (Eq, Ord, Show, Read)

derive makeNFData ''JSON


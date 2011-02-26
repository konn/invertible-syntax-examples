module JSONPretty (prettyJSON) where
import Text.PrettyPrint.HughesPJ
import Data.List

import JSON

prettyJSON :: JSON -> String
prettyJSON = render . sub
  where
    sub (Number int) = integer int
    sub Null         = text "null"
    sub (Bool b)     = case b of
                         True  -> text "true"
                         False -> text "false"
    sub (Array jss)  = brackets $ hcat (intersperse comma $ map sub jss)
    sub (String str) = doubleQuotes $ text str
    sub (Object jss) = braces $ hcat (intersperse comma $ map term jss)
      where term (key, js) = doubleQuotes (text key) <> colon <+> sub js

module Utils.Prefix
    ( setPrefix
    , setPrefixList
    , setPrefixTail
    ) where


import Data.Typeable
import Language.Haskell.Exts.Syntax
import Types


setPrefixTail ::
    (Annotated ast, Typeable ast)
    => String
    -> [ast NodeInfo]
    -> [ast NodeInfo]
setPrefixTail _ [] =
    []

setPrefixTail _ [ x ] =
    [ x ]

setPrefixTail prefix' (x : xs) =
    x : (map (setPrefix prefix') xs)


setPrefixList ::
    (Annotated ast, Typeable ast)
    => String
    -> [ast NodeInfo]
    -> [ast NodeInfo]
setPrefixList prefix' =
    map (setPrefix prefix')


setPrefix ::
    (Annotated ast, Typeable ast)
    => String
    -> ast NodeInfo
    -> ast NodeInfo
setPrefix prefix' =
    amap (\n -> n { prefix = prefix' })

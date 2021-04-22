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
setPrefixTail prefix nodes =
    case nodes of
        [] ->
            []

        [ x ] ->
            [ x ]

        (x : xs) ->
            x : map (setPrefix prefix) xs


setPrefixList ::
    (Annotated ast, Typeable ast)
    => String
    -> [ast NodeInfo]
    -> [ast NodeInfo]
setPrefixList prefix =
    map (setPrefix prefix)


setPrefix ::
    (Annotated ast, Typeable ast)
    => String
    -> ast NodeInfo
    -> ast NodeInfo
setPrefix prefix =
    amap (\n -> n { linePrefix = prefix })

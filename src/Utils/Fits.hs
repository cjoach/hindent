module Utils.Fits
    ( fitsOnOneLine_
    , ifFitsOnOneLineOrElse
    ) where


import Control.Applicative
import Control.Monad.State.Strict hiding (state)
import Types
import Utils.Flow


ifFitsOnOneLineOrElse :: Printer a -> Printer a -> Printer a
ifFitsOnOneLineOrElse a b = do
    aIsOneLine <- fitsOnOneLine_ a
    if aIsOneLine then
        a

    else
        b


fitsOnOneLine_ :: Printer a -> Printer Bool
fitsOnOneLine_ p = do
    st <- get
    put st { psFitOnOneLine = True }
    ok <- fmap (const True) p <|> return False
    put st
    guard <| ok || not (psFitOnOneLine st)
    return ok

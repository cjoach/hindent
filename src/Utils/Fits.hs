module Utils.Fits
    ( fitsOnOneLine
    , fitsOnOneLine_
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


fitsOnOneLine :: Printer a -> Printer (Maybe PrintState)
fitsOnOneLine p = do
    st <- get
    put st { psFitOnOneLine = True }
    ok <- fmap (const True) p <|> return False
    st' <- get
    put st
    guard <| ok || not (psFitOnOneLine st)
    return
        ( if ok then
            Just st' { psFitOnOneLine = psFitOnOneLine st }

          else
            Nothing
        )

module Utils.Fits
    ( ifFitsOnOneLineOrElse
    , fitsOnOneLine
    , fitsOnOneLine_
    ) where


import Control.Applicative
import Control.Monad.State.Strict hiding (state)
import qualified Data.ByteString.Builder as S
import Data.Foldable (for_, traverse_)
import Data.Int
import Data.List
import Data.Maybe
import Data.Typeable
import qualified Language.Haskell.Exts as P
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)
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
    put st {psFitOnOneLine = True}
    ok <- fmap (const True) p <|> return False
    put st
    guard <| ok || not (psFitOnOneLine st)
    return ok


fitsOnOneLine :: Printer a -> Printer (Maybe PrintState)
fitsOnOneLine p = do
    st <- get
    put st {psFitOnOneLine = True}
    ok <- fmap (const True) p <|> return False
    st' <- get
    put st
    guard <| ok || not (psFitOnOneLine st)
    return
        (if ok then
            Just st' {psFitOnOneLine = psFitOnOneLine st}

         else
            Nothing
        )

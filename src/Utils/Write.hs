module Utils.Write
    ( writeDo
    , writeMdo
    , writeLet
    , writeIn
    , rightArrow
    , leftArrow
    , rightFatArrow
    , nothing
    , space
    , comma
    , int
    , write
    , string
    , writeForall
    , newline
    , oneEmptyLine
    , twoEmptyLines
    ) where


import Control.Applicative
import Control.Monad.State.Strict hiding (state)
import Data.ByteString.Builder as S
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
import Utils.Fits
import Utils.Flow


write :: String -> Printer ()
write x = do
    let additionalLines = length (filter (== '\n') x)
    eol <- gets psEolComment
    hardFail <- gets psFitOnOneLine
    let addingNewline = eol && x /= "\n"
    when addingNewline newline
    state <- get
    let writingNewline = x == "\n"
        psColumnStart' =
            if psNewline state && not writingNewline then
                psIndentLevel state

            else
                psColumnStart state
        out :: String
        out =
            if psNewline state && not writingNewline then
                (replicate (fromIntegral (psIndentLevel state)) ' ') <> x

            else
                x
        psColumn' =
            if additionalLines > 0 then
                x
                    |> lines
                    |> reverse
                    |> take 1
                    |> concat
                    |> length
                    |> fromIntegral

            else
                out
                    |> length
                    |> fromIntegral
                    |> (+) (psColumn state)
        noAdditionalLines = additionalLines == 0
        notOverMaxColumn =
            psColumn' <= configMaxColumns (psConfig state)
        notOverMaxCodeColumn =
            (psColumn' - psColumnStart')
                <= configMaxCodeColumns (psConfig state)
    when
        hardFail
        (guard
            (noAdditionalLines
                && notOverMaxColumn && notOverMaxCodeColumn
            )
        )
    modify
        (\s ->
            s
                { psOutput = psOutput state <> S.stringUtf8 out
                , psNewline = False
                , psLine = psLine state + fromIntegral additionalLines
                , psEolComment = False
                , psColumn = psColumn'
                , psColumnStart = psColumnStart'
                }
        )


newline :: Printer ()
newline = do
    write "\n"
    modify (\s -> s {psNewline = True})


oneEmptyLine :: Printer ()
oneEmptyLine = do
    newline
    newline


twoEmptyLines :: Printer ()
twoEmptyLines = do
    newline
    newline
    newline


writeForall :: Printer ()
writeForall =
    write "forall"


writeDo :: Printer ()
writeDo =
    write "do"


writeMdo :: Printer ()
writeMdo =
    write "mdo"


writeLet :: Printer ()
writeLet =
    write "let"


writeIn :: Printer ()
writeIn =
    write "in"


rightArrow :: Printer ()
rightArrow =
    write "->"


leftArrow :: Printer ()
leftArrow =
    write "<-"


rightFatArrow :: Printer ()
rightFatArrow =
    write "=>"


space :: Printer ()
space =
    write " "


comma :: Printer ()
comma =
    write ","


int :: Integer -> Printer ()
int =
    write . show


string :: String -> Printer ()
string =
    write


nothing :: Printer ()
nothing =
    return ()

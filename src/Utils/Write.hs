module Utils.Write
    ( boxWrap
    , comma
    , emptyBraces
    , emptyBrackets
    , emptyParens
    , int
    , leftArrow
    , newline
    , nothing
    , oneEmptyLine
    , rightArrow
    , rightFatArrow
    , space
    , twoEmptyLines
    , write
    , writeCase
    , writeDo
    , writeForall
    , writeIf
    , writeLet
    , writeMdo
    , writeOf
    , writeWhere
    ) where


import Control.Monad.State.Strict hiding (state)
import Data.ByteString.Builder as S
import Data.List
import Language.Haskell.Exts.Syntax
import Prelude hiding (exp)
import Types
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
        notOverMaxColumn = psColumn' <= configMaxColumns (psConfig state)
        notOverMaxCodeColumn =
            (psColumn' - psColumnStart')
                <= configMaxCodeColumns (psConfig state)
    when hardFail
        (guard (noAdditionalLines && notOverMaxColumn && notOverMaxCodeColumn))
    modify
        ( \s ->
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
    modify (\s -> s { psNewline = True })


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


writeIf :: Printer ()
writeIf =
    write "if"


writeCase :: Printer ()
writeCase =
    write "case"


writeOf :: Printer ()
writeOf =
    write "of"


writeWhere :: Printer ()
writeWhere =
    write "where"


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


nothing :: Printer ()
nothing =
    return ()


boxWrap :: Boxed -> (String, String)
boxWrap boxed =
    case boxed of
        Boxed ->
            ( "(", ")" )

        Unboxed ->
            ( "(#", "#)" )


emptyParens :: Printer ()
emptyParens =
    write "()"


emptyBraces :: Printer ()
emptyBraces =
    write "{}"


emptyBrackets :: Printer ()
emptyBrackets =
    write "[]"

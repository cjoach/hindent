module Utils.Combinator
    ( identity
    , indented
    , indentedBlock
    , spaced
    , commas
    , inter
    , lined
    , doubleLined
    , prefixedLined
    , prefixedLined_
    , column
    , withCaseContext
    , withLetStatementContext
    , wrap
    , parens
    , braces
    , brackets
    , wrapSpaces
    , quotation
    , getIndentSpaces
    , depend
    ) where


import Control.Monad.State.Strict hiding (state)
import Data.Int
import Data.List
import Types
import Utils.Fits
import Utils.Flow
import Utils.Write


identity :: Printer a -> Printer a
identity printer =
    printer


wrap :: String -> String -> Printer a -> Printer a
wrap open close p =
    let
        oneline = do
            write open
            p' <- p
            write close
            return p'

        multiline = do
            write open
            p' <- indented 1 p
            newline
            write close
            return p'
    in
    ifFitsOnOneLineOrElse oneline multiline


parens :: Printer a -> Printer a
parens =
    wrap "(" ")"


braces :: Printer a -> Printer a
braces =
    wrap "{" "}"


brackets :: Printer a -> Printer a
brackets =
    wrap "[" "]"


wrapSpaces :: Printer a -> Printer a
wrapSpaces =
    wrap " " " "


-- | Write a Template Haskell quotation or a quasi-quotation.
--
-- >>> quotation "t" (string "Foo")
-- > [t|Foo|]
quotation :: String -> Printer () -> Printer ()
quotation quoter p =
    let
        open =
            "[" ++ quoter ++ "|"

        close =
            "|]"

        oneline = do
            write open
            p' <- p
            write close
            return p'

        multiline = do
            write open
            p' <- indented 1 p
            write close
            return p'
    in
    ifFitsOnOneLineOrElse oneline multiline


getIndentSpaces :: Printer Int64
getIndentSpaces =
    gets (configIndentSpaces . psConfig)


-- | Increase indentation level by n spaces for the given printer.
indented :: Int64 -> Printer a -> Printer a
indented i p = do
    level <- gets psIndentLevel
    modify (\s -> s { psIndentLevel = level + i })
    m <- p
    modify (\s -> s { psIndentLevel = level })
    return m


indentedBlock :: Printer a -> Printer a
indentedBlock p = do
    level <- gets psIndentLevel
    indentSpaces <- getIndentSpaces
    let newIndent = indentSpaces - (level `rem` indentSpaces)
    indented newIndent p


-- | Make the latter's indentation depend upon the end column of the
-- former.
depend :: Printer () -> Printer b -> Printer b
depend maker dependent = do
    state' <- get
    maker
    st <- get
    col <- gets psColumn
    if psLine state' /= psLine st || psColumn state' /= psColumn st then
        column col dependent

    else
        dependent


-- | Print all the printers separated by spaces.
spaced :: [Printer ()] -> Printer ()
spaced =
    inter space


-- | Print all the printers separated by commas.
commas :: [Printer ()] -> Printer ()
commas =
    inter (write ", ")


-- | Print all the printers separated by sep.
inter :: Printer () -> [Printer ()] -> Printer ()
inter sep ps =
    foldr
        (\(i, p) next ->
            depend
                (do
                    p
                    if i < length ps then
                        sep

                    else
                        return ()
                )
                next
        )
        (return ())
        (zip [1 ..] ps)


-- | Print all the printers separated by newlines.
lined :: [Printer ()] -> Printer ()
lined ps =
    ps
        |> intersperse newline
        |> sequence_


-- | Print all the printers separated by newlines.
doubleLined :: [Printer ()] -> Printer ()
doubleLined ps =
    ps
        |> intersperse (newline >> newline)
        |> sequence_


-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined :: String -> [Printer ()] -> Printer ()
prefixedLined pref ps' =
    case ps' of
        [] ->
            return ()

        (p:ps) -> do
            p
            indented (fromIntegral (length pref * (-1)))
                (mapM_
                    (\p' -> do
                        newline
                        depend (write pref) p'
                    )
                    ps
                )


-- | Print all the printers separated newlines and optionally a line
-- prefix.
prefixedLined_ :: String -> [Printer ()] -> Printer ()
prefixedLined_ pref ps' =
    case ps' of
        [] ->
            return ()

        (p:ps) -> do
            p
            mapM_
                (\p' -> do
                    newline
                    write pref
                    p'
                )
                ps


-- | Set the (newline-) indent level to the given column for the given
-- printer.
column :: Int64 -> Printer a -> Printer a
column i p = do
    level <- gets psIndentLevel
    modify (\s -> s { psIndentLevel = i })
    m <- p
    modify (\s -> s { psIndentLevel = level })
    return m


-- | Output a newline.
-- | Set the context to a case context, where RHS is printed with -> .
withCaseContext :: Bool -> Printer a -> Printer a
withCaseContext bool pr = do
    original <- gets psInsideCase
    modify (\s -> s { psInsideCase = bool })
    result <- pr
    modify (\s -> s { psInsideCase = original })
    return result


withLetStatementContext :: Bool -> Printer a -> Printer a
withLetStatementContext bool pr = do
    original <- gets psInsideLetStatement
    modify (\s -> s { psInsideLetStatement = bool })
    result <- pr
    modify (\s -> s { psInsideLetStatement = original })
    return result

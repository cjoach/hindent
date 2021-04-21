{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}


-- | Pretty printing.
module Pretty
    ( pretty
    ) where


import Control.Monad.State.Strict hiding (state)
import Data.Char
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
import Utils.Combinator
import Utils.Fits
import Utils.Flow
import Utils.Prefix
import Utils.Write


--------------------------------------------------------------------------------
-- * Pretty printing class
-- | Pretty printing class.
class (Annotated ast, Typeable ast) =>
      Pretty ast
    where
    prettyInternal :: ast NodeInfo -> Printer ()


-- | Pretty print including comments.
pretty :: (Pretty ast, Show (ast NodeInfo)) => ast NodeInfo -> Printer ()
pretty a = do
    comments
        |> mapM_
            (\case
                CommentBeforeLine _ c -> do
                    writeComment c
                    newline

                _ ->
                    return ()
            )
    a
        |> ann
        |> prefix
        |> write
    prettyInternal a
    mapM_
        (\( i, c' ) -> do
            case c' of
                CommentSameLine _ c -> do
                    space
                    writeComment c

                CommentAfterLine _ c -> do
                    when (i == 0) newline
                    writeComment c

                _ ->
                    return ()
        )
        (zip [0 :: Int ..] comments)
    where
        comments =
            nodeInfoComments (ann a)

        writeComment =
            \case
                EndOfLine cs -> do
                    write ("--" ++ cs)
                    modify (\s -> s { psEolComment = True })

                MultiLine cs -> do
                    write ("{-" ++ cs ++ "-}")
                    modify (\s -> s { psEolComment = True })


-- | Pretty print using HSE's own printer. The 'P.Pretty' class here
-- is HSE's.
pretty' ::
    (Pretty ast, P.Pretty (ast SrcSpanInfo))
    => ast NodeInfo
    -> Printer ()
pretty' =
    write . P.prettyPrint . fmap nodeInfoSpan


-- | Get the current RHS separator, either = or -> .
rhsSeparator :: Printer ()
rhsSeparator = do
    inCase <- gets psInsideCase
    if inCase then
        write "->"

    else
        write "="


-- -- | Play with a printer and then restore the state to what it was
-- -- before.
-- sandbox :: Printer a -> Printer (a, PrintState)
-- sandbox p = do
--     orig <- get
--     a <- p
--     new <- get
--     put orig
--     return (a, new)
-- | Render a type with a context, or not.
withCtx ::
    (Pretty ast, Show (ast NodeInfo))
    => Maybe (ast NodeInfo)
    -> Printer b
    -> Printer b
withCtx Nothing m =
    m
withCtx (Just ctx) m = do
    pretty ctx
    write " =>"
    newline
    m


-- | Maybe render an overlap definition.
maybeOverlap :: Maybe (Overlap NodeInfo) -> Printer ()
maybeOverlap =
    maybe (return ()) (\p -> pretty p >> space)


-- | Swing the second printer below and indented
swing :: Printer () -> Printer b -> Printer ()
swing a b = do
    a
    mst <-
        fitsOnOneLine <| do
            space
            b
    case mst of
        Just st ->
            put st

        Nothing -> do
            newline
            _ <- indentedBlock b
            return ()


--------------------------------------------------------------------------------
-- * Instances
instance Pretty Context where
    prettyInternal ctx@(CxTuple _ asserts) =
        let
            horizontal = do
                write "("
                asserts
                    |> map pretty
                    |> commas
                write ")"

            vertical =
                context ctx
        in
        case asserts of
            [] ->
                emptyParens

            _ ->
                ifFitsOnOneLineOrElse horizontal vertical
    prettyInternal ctx =
        context ctx


instance Pretty Pat where
    prettyInternal x =
        case x of
            PLit _ sign l ->
                pretty sign >> pretty l

            PNPlusK _ n k ->
                depend
                    (do
                        pretty n
                        write "+"
                    )
                    (int k)

            PInfixApp _ a op b ->
                case op of
                    Special {} ->
                        depend (pretty a) (depend (prettyInfixOp op) (pretty b))

                    _ ->
                        depend
                            (do
                                pretty a
                                space
                            )
                            (depend
                                (do
                                    prettyInfixOp op
                                    space
                                )
                                (pretty b)
                            )

            PApp _ f args ->
                depend
                    (do
                        pretty f
                        unless (null args) space
                    )
                    (spaced (map pretty args))

            PTuple _ boxed pats ->
                let
                    ( open, close ) =
                        boxWrap boxed
                in
                case pats of
                    [] ->
                        emptyParens

                    _ -> do
                        write open
                        space
                        pats
                            |> map pretty
                            |> commas
                        space
                        write close

            PList _ ps ->
                case ps of
                    [] ->
                        emptyBrackets

                    _ -> do
                        write "["
                        space
                        ps
                            |> map pretty
                            |> commas
                        space
                        write "]"

            PParen _ e -> do
                write "("
                pretty e
                write ")"

            PRec _ qname fields ->
                let
                    horizontal = do
                        write "{"
                        space
                        fields
                            |> map pretty
                            |> commas
                        space
                        write "}"

                    vertical = do
                        write "{"
                        space
                        fields
                            |> setPrefixTail ", "
                            |> map pretty
                            |> lined
                        newline
                        write "}"
                in do
                    pretty qname
                    space
                    case fields of
                        [] ->
                            emptyBraces

                        _ ->
                            ifFitsOnOneLineOrElse horizontal vertical

            PAsPat _ n p ->
                depend
                    (do
                        pretty n
                        write "@"
                    )
                    (pretty p)

            PWildCard _ ->
                write "_"

            PIrrPat _ p ->
                depend (write "~") (pretty p)

            PatTypeSig _ p ty ->
                depend
                    (do
                        pretty p
                        write " :: "
                    )
                    (pretty ty)

            PViewPat _ e p ->
                depend
                    (do
                        pretty e
                        write " -> "
                    )
                    (pretty p)

            PQuasiQuote _ name str ->
                quotation name (string str)

            PBangPat _ p ->
                depend (write "!") (pretty p)

            PRPat {} ->
                pretty' x

            PXTag {} ->
                pretty' x

            PXETag {} ->
                pretty' x

            PXPcdata {} ->
                pretty' x

            PXPatTag {} ->
                pretty' x

            PXRPats {} ->
                pretty' x

            PVar {} ->
                pretty' x

            PSplice _ s ->
                pretty s


-- | Pretty infix application of a name (identifier or symbol).
prettyInfixName :: Name NodeInfo -> Printer ()
prettyInfixName (Ident _ n) = do
    write "`"
    string n
    write "`"
prettyInfixName (Symbol _ s) =
    string s


-- | Pretty print a name for being an infix operator.
prettyInfixOp :: QName NodeInfo -> Printer ()
prettyInfixOp x =
    case x of
        Qual _ mn n ->
            case n of
                Ident _ i -> do
                    write "`"
                    pretty mn
                    write "."
                    string i
                    write "`"

                Symbol _ s -> do
                    pretty mn
                    write "."
                    string s

        UnQual _ n ->
            prettyInfixName n

        Special _ s ->
            pretty s


prettyQuoteName :: Name NodeInfo -> Printer ()
prettyQuoteName x =
    case x of
        Ident _ i ->
            string i

        Symbol _ s ->
            string ("(" ++ s ++ ")")


instance Pretty Type where
    prettyInternal =
        typ


instance Pretty Exp where
    prettyInternal =
        exp


-- | Render an expression.
exp :: Exp NodeInfo -> Printer ()
-- | Do after lambda should swing.
exp (Lambda _ pats (Do l stmts)) = do
    mst <-
        fitsOnOneLine
            (do
                write "\\"
                spaced (map pretty pats)
                write " -> "
                pretty (Do l stmts)
            )
    case mst of
        Nothing ->
            swing
                (do
                    write "\\"
                    spaced (map pretty pats)
                    write " -> do"
                )
                (lined (map pretty stmts))

        Just st ->
            put st
-- | Space out tuples.
exp (Tuple _ boxed exps) =
    let
        ( open, close ) =
            boxWrap boxed

        horizontal = do
            write open
            space
            exps
                |> map pretty
                |> commas
            space
            write close

        vertical = do
            write open
            space
            exps
                |> setPrefixTail ", "
                |> map pretty
                |> lined
            newline
            write close
    in
    case exps of
        [] ->
            emptyParens

        _ ->
            ifFitsOnOneLineOrElse horizontal vertical
-- | Space out tuples.
exp (TupleSection _ boxed mexps) =
    let
        ( open, close ) =
            boxWrap boxed
    in do
        write open
        space
        mexps
            |> map (maybe nothing pretty)
            |> commas
        space
        write close
exp (UnboxedSum {}) =
    error "FIXME: No implementation for UnboxedSum."
-- | Infix apps, same algorithm as ChrisDone at the moment.
exp e@(InfixApp _ a op b) =
    infixApp e a op b
-- | If bodies are indented 4 spaces. Handle also do-notation.
exp (If _ if' then' else') =
    let
        ifLine = do
            write "if"
            space
            pretty if'
            space
            write "then"
            potentialDo then'

        potentialDo expression =
            case expression of
                Do _ _ -> do
                    space
                    writeDo

                _ ->
                    return ()

        printExpression expression =
            case expression of
                Do _ _ -> do
                    space
                    pretty expression

                If _ _ _ _ -> do
                    space
                    pretty expression

                _ -> do
                    newline
                    indentedBlock (pretty expression)
    in do
        isOneLiner <- fitsOnOneLine_ ifLine
        write "if"
        if isOneLiner then
            space >> pretty if' >> space

        else do
            newline
            indentedBlock (pretty if')
            newline
        write "then"
        printExpression then'
        oneEmptyLine
        write "else"
        printExpression else'
-- | Render on one line, or otherwise render the op with the arguments
-- listed line by line.
exp expression@(App _ op arg) =
    let
        flatten (App label' op' arg') =
            flatten op' ++ [ amap (addComments label') arg' ]
        flatten x =
            [ x ]

        addComments n1 n2 =
            n2
                { nodeInfoComments =
                    nub (nodeInfoComments n2 ++ nodeInfoComments n1)
                }

        flattened =
            flatten op ++ [ arg ]

        isBreakFromFile =
            srcSpanStartLine srcSpan /= srcSpanEndLine srcSpan

        srcSpan =
            ann expression
                |> nodeInfoSpan
                |> srcInfoSpan

        oneLine = do
            flattened
                |> map pretty
                |> spaced

        firstArgOneLine = do
            let (f : a1 : _) = flattened
            pretty f
            space
            pretty a1

        firstArgMultiline = do
            let (f : a1 : args) = flattened
            pretty f
            space
            pretty a1
            newline
            args
                |> map pretty
                |> lined
                |> indentedBlock

        multiline = do
            let (f : args) = flattened
            pretty f
            newline
            args
                |> map pretty
                |> lined
                |> indentedBlock
    in do
        isOneLine <- fitsOnOneLine_ oneLine
        isFirstArgOneLine <- fitsOnOneLine_ firstArgOneLine
        if isBreakFromFile && isFirstArgOneLine then
            firstArgMultiline

        else if isBreakFromFile && (not isFirstArgOneLine) then
            multiline

        else if isOneLine then
            oneLine

        else if isFirstArgOneLine then
            firstArgMultiline

        else
            multiline
-- | Space out commas in list.
exp (List _ es) =
    let
        horizontal = do
            write "["
            space
            es
                |> map pretty
                |> commas
            space
            write "]"

        vertical = do
            write "["
            space
            es
                |> setPrefixTail ", "
                |> map pretty
                |> lined
            newline
            write "]"
    in
    case es of
        [] ->
            emptyBrackets

        _ ->
            ifFitsOnOneLineOrElse horizontal vertical
exp e@(RecUpdate _ exp' updates) =
    recUpdateExpr e (pretty exp') updates
exp e@(RecConstr _ qname updates) =
    recUpdateExpr e (pretty qname) updates
exp (Let _ binds e) =
    let
        afterIn =
            case e of
                Do _ _ -> do
                    space
                    pretty e

                _ -> do
                    newline
                    pretty e
    in do
        writeLet
        newline
        indentedBlock (pretty binds)
        newline
        writeIn
        afterIn
exp (ListComp _ e qstmt) =
    let
        horizontal = do
            write "["
            space
            pretty e
            space
            write "|"
            space
            qstmt
                |> map pretty
                |> commas
            space
            write "]"

        vertical = do
            write "["
            space
            pretty e
            newline
            write "|"
            space
            qstmt
                |> setPrefixTail ", "
                |> map pretty
                |> lined
            newline
            write "]"
    in
    case qstmt of
        [] ->
            emptyBrackets

        _ ->
            ifFitsOnOneLineOrElse horizontal vertical
exp (ParComp _ e qstmts) =
    let
        checkEmpty fn qstmt =
            case qstmt of
                [] ->
                    emptyBrackets

                _ ->
                    fn qstmt

        horizontalQstmt qstmt = do
            space
            write "|"
            space
            qstmt
                |> map pretty
                |> commas

        verticalQstmt qstmt = do
            newline
            write "|"
            space
            qstmt
                |> setPrefixTail ", "
                |> map pretty
                |> lined
            newline
            write "]"

        horizontal = do
            write "["
            space
            pretty e
            for_ qstmts <| checkEmpty horizontalQstmt
            space
            write "]"

        vertical = do
            write "["
            space
            pretty e
            for_ qstmts <| checkEmpty verticalQstmt
            newline
            write "]"
    in
    case qstmts of
        [] ->
            emptyBrackets

        _ ->
            ifFitsOnOneLineOrElse horizontal vertical
exp (TypeApp _ t) = do
    write "@"
    pretty t
exp (NegApp _ e) =
    depend (write "-") (pretty e)
exp (Lambda _ ps e) = do
    write "\\"
    spaced
        [ do
            case ( i, x ) of
                ( 0, PIrrPat {} ) ->
                    space

                ( 0, PBangPat {} ) ->
                    space

                _ ->
                    return ()
            pretty x
        | ( i, x ) <- zip [0 :: Int ..] ps
        ]
    swing (write " ->") <| pretty e
exp (Paren _ e) =
    let
        horizontal = do
            write "("
            pretty e
            write ")"

        vertical = do
            write "("
            space
            indented 2 <| pretty e
            newline
            write ")"
    in
    ifFitsOnOneLineOrElse horizontal vertical
exp (Case _ e alts) =
    let
        horizontal = do
            writeCase
            space
            pretty e
            space
            writeOf

        vertical = do
            writeCase
            newline
            indentedBlock <| pretty e
            newline
            writeOf

        emptyAlternatives = do
            space
            emptyBraces

        nonEmptyAlternatives = do
            newline
            alts
                |> map (withCaseContext True . pretty)
                |> doubleLined
                |> indentedBlock
    in do
        ifFitsOnOneLineOrElse horizontal vertical
        if null alts then
            emptyAlternatives

        else
            nonEmptyAlternatives
exp (Do _ statements) =
    let
        srcSpan =
            srcInfoSpan . nodeInfoSpan . ann

        checkNewlineFromSrc x y =
            let
                n =
                    (srcSpanStartLine (srcSpan y))
                        - (srcSpanEndLine (srcSpan x))
            in
            repeat newline
                |> take n
                |> sequence_

        statementPrinters =
            statements
                |> map pretty

        separatorPrinters =
            zipWith checkNewlineFromSrc statements (tail statements)

        fullList =
            [ statementPrinters, separatorPrinters ]
                |> transpose
                |> concat
                |> sequence_
    in do
        writeDo
        newline
        indentedBlock fullList
exp (MDo _ statements) = do
    writeMdo
    newline
    statements
        |> map pretty
        |> lined
        |> indentedBlock
exp (LeftSection _ e op) =
    let
        horizontal = do
            write "("
            pretty e
            space
            pretty op
            write ")"

        vertical = do
            write "("
            space
            pretty e
            newline
            indentedBlock <| pretty op
            newline
            write ")"
    in
    ifFitsOnOneLineOrElse horizontal vertical
exp (RightSection _ e op) =
    let
        horizontal = do
            write "("
            pretty e
            space
            pretty op
            write ")"

        vertical = do
            write "("
            space
            pretty e
            newline
            indentedBlock <| pretty op
            newline
            write ")"
    in
    ifFitsOnOneLineOrElse horizontal vertical
exp (EnumFrom _ e) = do
    write "["
    space
    pretty e
    space
    write ".."
    space
    write "]"
exp (EnumFromTo _ e f) = do
    write "["
    space
    pretty e
    space
    write ".."
    space
    pretty f
    space
    write "]"
exp (EnumFromThen _ e t) = do
    write "["
    space
    pretty e
    write ","
    space
    pretty t
    space
    write ".."
    space
    write "]"
exp (EnumFromThenTo _ e t f) = do
    write "["
    space
    pretty e
    write ","
    space
    pretty t
    space
    write ".."
    space
    pretty f
    space
    write "]"
exp (ExpTypeSig _ e t) =
    depend
        (do
            pretty e
            write " :: "
        )
        (pretty t)
exp (VarQuote _ x) =
    depend (write "'") (pretty x)
exp (TypQuote _ x) =
    depend (write "''") (pretty x)
exp (BracketExp _ b) =
    pretty b
exp (SpliceExp _ s) =
    pretty s
exp (QuasiQuote _ n s) =
    quotation n (string s)
exp (LCase _ alts) = do
    write "\\case"
    if null alts then do
        space
        emptyBraces

    else do
        newline
        alts
            |> map (withCaseContext True . pretty)
            |> doubleLined
            |> indentedBlock
exp (MultiIf _ alts) =
    let
        prettyG (GuardedRhs _ stmts e) =
            let
                fn =
                    \( i, p ) -> do
                        unless (i == 1) space
                        pretty p
                        unless (i == length stmts) (write ",")
            in do
                stmts
                    |> zip [1 ..]
                    |> map fn
                    |> lined
                    |> indented 1
                swing (write " " >> rhsSeparator) (pretty e)
    in
    withCaseContext True
        (depend (write "if ")
            (lined
                (map
                    (\p -> do
                        write "| "
                        prettyG p
                    )
                    alts
                )
            )
        )
exp (Lit _ lit) =
    prettyInternal lit
exp (Var _ q) =
    pretty q
exp (IPVar _ q) =
    pretty q
exp (Con _ q) =
    pretty q
exp x@XTag {} =
    pretty' x
exp x@XETag {} =
    pretty' x
exp x@XPcdata {} =
    pretty' x
exp x@XExpTag {} =
    pretty' x
exp x@XChildTag {} =
    pretty' x
exp x@CorePragma {} =
    pretty' x
exp x@SCCPragma {} =
    pretty' x
exp x@GenPragma {} =
    pretty' x
exp x@Proc {} =
    pretty' x
exp x@LeftArrApp {} =
    pretty' x
exp x@RightArrApp {} =
    pretty' x
exp x@LeftArrHighApp {} =
    pretty' x
exp x@RightArrHighApp {} =
    pretty' x
exp x@ParArray {} =
    pretty' x
exp x@ParArrayFromTo {} =
    pretty' x
exp x@ParArrayFromThenTo {} =
    pretty' x
exp x@ParArrayComp {} =
    pretty' x
exp (OverloadedLabel _ label) =
    string ('#' : label)


instance Pretty IPName where
    prettyInternal =
        pretty'


instance Pretty Stmt where
    prettyInternal =
        stmt


instance Pretty QualStmt where
    prettyInternal x =
        case x of
            QualStmt _ s ->
                pretty s

            ThenTrans _ s -> do
                write "then "
                pretty s

            ThenBy _ s t -> do
                write "then "
                pretty s
                write " by "
                pretty t

            GroupBy _ s -> do
                write "then group by "
                pretty s

            GroupUsing _ s -> do
                write "then group using "
                pretty s

            GroupByUsing _ s t -> do
                write "then group by "
                pretty s
                write " using "
                pretty t


instance Pretty Decl where
    prettyInternal =
        decl'


-- | Render a declaration.
decl :: Decl NodeInfo -> Printer ()
decl (InstDecl _ moverlap dhead decls) = do
    depend (write "instance ")
        (depend (maybeOverlap moverlap)
            (depend (pretty dhead)
                (unless (null (fromMaybe [] decls)) (write " where"))
            )
        )
    unless (null (fromMaybe [] decls))
        (do
            newline
            indentedBlock (lined (map pretty (fromMaybe [] decls)))
        )
decl (SpliceDecl _ e) =
    pretty e
decl (TypeSig _ names ty) =
    depend
        (do
            inter (write ", ") (map pretty names)
            write " :: "
        )
        (pretty ty)
decl (FunBind _ matches) =
    lined (map pretty matches)
decl (ClassDecl _ ctx dhead fundeps decls) = do
    classHead ctx dhead fundeps decls
    unless (null (fromMaybe [] decls))
        (do
            newline
            indentedBlock (lined (map pretty (fromMaybe [] decls)))
        )
decl (TypeDecl _ typehead typ') = do
    write "type"
    space
    pretty typehead
    space
    write "="
    newline
    indentedBlock (pretty typ')
decl (TypeFamDecl _ declhead result injectivity) = do
    write "type family "
    pretty declhead
    case result of
        Just r -> do
            space
            let sep =
                    case r of
                        KindSig _ _ ->
                            "::"

                        TyVarSig _ _ ->
                            "="
            write sep
            space
            pretty r

        Nothing ->
            return ()
    case injectivity of
        Just i -> do
            space
            pretty i

        Nothing ->
            return ()
decl (ClosedTypeFamDecl _ declhead result injectivity instances) = do
    write "type family "
    pretty declhead
    for_ result <|
        \r -> do
            space
            let sep =
                    case r of
                        KindSig _ _ ->
                            "::"

                        TyVarSig _ _ ->
                            "="
            write sep
            space
            pretty r
    for_ injectivity <|
        \i -> do
            space
            pretty i
    space
    write "where"
    newline
    indentedBlock (lined (map pretty instances))
decl (DataDecl _ dataornew ctx dhead condecls mderivs) = do
    depend
        (do
            pretty dataornew
            space
        )
        (withCtx ctx
            (do
                pretty dhead
                case ( dataornew, condecls ) of
                    ( _, [] ) ->
                        return ()

                    ( NewType _, [x] ) ->
                        singleCons x

                    ( _, xs ) ->
                        multiCons xs
            )
        )
    indentSpaces <- getIndentSpaces
    forM_ mderivs <|
        \deriv -> newline >> column indentSpaces (pretty deriv)
    where
        singleCons x = do
            write " ="
            indentSpaces <- getIndentSpaces
            column indentSpaces
                (do
                    newline
                    pretty x
                )

        multiCons xs = do
            newline
            indentSpaces <- getIndentSpaces
            column indentSpaces
                (depend (write "=")
                    (prefixedLined "|" (map (depend space . pretty) xs))
                )
decl (GDataDecl _ dataornew ctx dhead mkind condecls mderivs) = do
    depend (pretty dataornew >> space)
        (withCtx ctx
            (do
                pretty dhead
                case mkind of
                    Nothing ->
                        return ()

                    Just kind -> do
                        write " :: "
                        pretty kind
                write " where"
            )
        )
    indentedBlock <| do
        case condecls of
            [] ->
                return ()

            _ -> do
                newline
                lined (map pretty condecls)
        forM_ mderivs <| \deriv -> newline >> pretty deriv
decl (InlineSig _ inline active name) = do
    write "{-# "
    unless inline <| write "NO"
    write "INLINE "
    case active of
        Nothing ->
            return ()

        Just (ActiveFrom _ x) ->
            write ("[" ++ show x ++ "] ")

        Just (ActiveUntil _ x) ->
            write ("[~" ++ show x ++ "] ")
    pretty name
    write " #-}"
decl (MinimalPragma _ (Just formula)) = do
    write "{-#"
    space
    write "MINIMAL"
    space
    pretty formula
    space
    write "#-}"
decl (ForImp _ callconv maybeSafety maybeName name ty) = do
    string "foreign import "
    pretty' callconv >> space
    case maybeSafety of
        Just safety ->
            pretty' safety >> space

        Nothing ->
            return ()
    case maybeName of
        Just namestr ->
            string (show namestr) >> space

        Nothing ->
            return ()
    pretty' name
    tyline <-
        fitsOnOneLine <| do
            string " :: "
            pretty' ty
    case tyline of
        Just line ->
            put line

        Nothing -> do
            newline
            indentedBlock <| do
                string ":: "
                pretty' ty
decl (ForExp _ callconv maybeName name ty) = do
    string "foreign export "
    pretty' callconv >> space
    case maybeName of
        Just namestr ->
            string (show namestr) >> space

        Nothing ->
            return ()
    pretty' name
    tyline <-
        fitsOnOneLine <| do
            string " :: "
            pretty' ty
    case tyline of
        Just line ->
            put line

        Nothing -> do
            newline
            indentedBlock <| do
                string ":: "
                pretty' ty
decl x' =
    pretty' x'


classHead ::
    Maybe (Context NodeInfo)
    -> DeclHead NodeInfo
    -> [FunDep NodeInfo]
    -> Maybe [ClassDecl NodeInfo]
    -> Printer ()
classHead ctx dhead fundeps decls =
    shortHead `ifFitsOnOneLineOrElse` longHead
    where
        shortHead =
            depend (write "class ")
                (withCtx ctx <|
                    depend (pretty dhead)
                        (depend
                            (unless (null fundeps)
                                (write " | " >> commas (map pretty fundeps))
                            )
                            (unless (null (fromMaybe [] decls)) (write " where")
                            )
                        )
                )

        longHead = do
            depend (write "class ") (withCtx ctx <| pretty dhead)
            newline
            indentedBlock <| do
                unless (null fundeps) <| do
                    depend (write "| ")
                        (prefixedLined ", " <| map pretty fundeps)
                    newline
                unless (null (fromMaybe [] decls)) (write "where")


instance Pretty TypeEqn where
    prettyInternal (TypeEqn _ in_ out_) = do
        pretty in_
        write " = "
        pretty out_


instance Pretty Deriving where
    prettyInternal (Deriving _ strategy heads) =
        let
            stripParens (IParen _ iRule) =
                stripParens iRule
            stripParens x =
                x

            heads' =
                if length heads == 1 then
                    map stripParens heads

                else
                    heads

            derive =
                case strategy of
                    Nothing ->
                        write "deriving"

                    Just st -> do
                        write "deriving"
                        space
                        pretty st

            horizontal = do
                derive
                space
                write "("
                heads'
                    |> map pretty
                    |> commas
                write ")"

            vertical = do
                derive
                newline
                indentedBlock <| do
                    write "("
                    space
                    heads'
                        |> setPrefixTail ", "
                        |> map pretty
                        |> lined
                    newline
                    write ")"
        in
        case heads of
            [] ->
                nothing

            _ ->
                ifFitsOnOneLineOrElse horizontal vertical


instance Pretty DerivStrategy where
    prettyInternal x =
        case x of
            DerivStock _ ->
                return ()

            DerivAnyclass _ ->
                write "anyclass"

            DerivNewtype _ ->
                write "newtype"


instance Pretty Alt where
    prettyInternal x =
        case x of
            Alt _ p galts mbinds -> do
                pretty p
                case galts of
                    GuardedRhss _ _ -> do
                        newline
                        indentedBlock <| pretty galts

                    UnGuardedRhs _ (Do _ _) -> do
                        space
                        write "->"
                        space
                        pretty galts

                    _ -> do
                        space
                        write "->"
                        newline
                        indentedBlock (pretty galts)
                case mbinds of
                    Nothing ->
                        return ()

                    Just binds -> do
                        newline
                        indentedBlock (depend (write "where ") (pretty binds))


instance Pretty Asst where
    prettyInternal x =
        case x of
            IParam _ name ty -> do
                pretty name
                write " :: "
                pretty ty

            ParenA _ asst -> do
                write "("
                pretty asst
                write ")"

            TypeA _ ty ->
                pretty ty


instance Pretty BangType where
    prettyInternal x =
        case x of
            BangedTy _ ->
                write "!"

            LazyTy _ ->
                write "~"

            NoStrictAnnot _ ->
                return ()


instance Pretty Unpackedness where
    prettyInternal (Unpack _) =
        write "{-# UNPACK #-}"
    prettyInternal (NoUnpack _) =
        write "{-# NOUNPACK #-}"
    prettyInternal (NoUnpackPragma _) =
        return ()


instance Pretty Binds where
    prettyInternal x =
        case x of
            BDecls _ ds ->
                formatBDecls ds

            IPBinds _ i ->
                lined (map pretty i)


formatBDecls :: [Decl NodeInfo] -> Printer ()
formatBDecls [x] =
    pretty x
formatBDecls (x : xs) =
    let
        separator =
            case x of
                TypeSig _ _ _ ->
                    newline

                _ -> do
                    isInLetStatement <- gets psInsideLetStatement
                    if isInLetStatement then
                        newline

                    else do
                        newline
                        newline
    in do
        pretty x
        separator
        formatBDecls xs


instance Pretty ClassDecl where
    prettyInternal x =
        case x of
            ClsDecl _ d ->
                pretty d

            ClsDataFam _ ctx h mkind ->
                depend (write "data ") <|
                    withCtx ctx <| do
                    pretty h
                    case mkind of
                        Nothing ->
                            return ()

                        Just kind -> do
                            write " :: "
                            pretty kind

            ClsTyFam _ h msig minj ->
                depend (write "type ")
                    (depend (pretty h)
                        (depend
                            (traverse_
                                (\case
                                    KindSig _ kind ->
                                        write " :: " >> pretty kind

                                    TyVarSig _ tyVarBind ->
                                        write " = " >> pretty tyVarBind
                                )
                                msig
                            )
                            (traverse_ (\inj -> space >> pretty inj) minj)
                        )
                    )

            ClsTyDef _ (TypeEqn _ this that) -> do
                write "type "
                pretty this
                write " = "
                pretty that

            ClsDefSig _ name ty -> do
                write "default "
                pretty name
                write " :: "
                pretty ty


instance Pretty ConDecl where
    prettyInternal x =
        conDecl x


instance Pretty FieldDecl where
    prettyInternal (FieldDecl _ names ty) =
        depend
            (do
                commas (map pretty names)
                write " :: "
            )
            (pretty ty)


instance Pretty FieldUpdate where
    prettyInternal x =
        case x of
            FieldUpdate _ n e ->
                swing
                    (do
                        pretty n
                        write " ="
                    )
                    (pretty e)

            FieldPun _ n ->
                pretty n

            FieldWildcard _ ->
                write ".."


instance Pretty GuardedRhs where
    prettyInternal =
        guardedRhs


instance Pretty InjectivityInfo where
    prettyInternal x =
        pretty' x


instance Pretty InstDecl where
    prettyInternal i =
        case i of
            InsDecl _ d ->
                pretty d

            InsType _ name ty ->
                depend
                    (do
                        write "type "
                        pretty name
                        write " = "
                    )
                    (pretty ty)

            _ ->
                pretty' i


instance Pretty Match where
    prettyInternal =
        match


instance Pretty PatField where
    prettyInternal x =
        case x of
            PFieldPat _ n p ->
                depend
                    (do
                        pretty n
                        write " = "
                    )
                    (pretty p)

            PFieldPun _ n ->
                pretty n

            PFieldWildcard _ ->
                write ".."


instance Pretty QualConDecl where
    prettyInternal x =
        case x of
            QualConDecl _ tyvars ctx d ->
                depend
                    (unless (null (fromMaybe [] tyvars))
                        (do
                            write "forall "
                            spaced (map pretty (reverse (fromMaybe [] tyvars)))
                            write ". "
                        )
                    )
                    (withCtx ctx (pretty d))


instance Pretty GadtDecl where
    prettyInternal (GadtDecl _ name _ _ fields t) =
        horVar `ifFitsOnOneLineOrElse` verVar
        where
            fields' p =
                case fromMaybe [] fields of
                    [] ->
                        return ()

                    fs -> do
                        write "{"
                        space
                        fs
                            |> setPrefixTail ", "
                            |> map pretty
                            |> lined
                        newline
                        write "}"
                        p

            horVar =
                depend (pretty name >> write " :: ") <| do
                    fields' (write " -> ")
                    declTy False t

            verVar = do
                pretty name
                newline
                indentedBlock <|
                    depend (write ":: ") <| do
                    fields' <| do
                        newline
                        write "-> "
                    declTy True t


instance Pretty Rhs where
    prettyInternal =
        rhs


instance Pretty Splice where
    prettyInternal x =
        case x of
            IdSplice _ str -> do
                write "$"
                string str

            ParenSplice _ e ->
                let
                    horizontal = do
                        write "$("
                        space
                        pretty e
                        space
                        write ")"

                    vertical = do
                        write "$("
                        space
                        pretty e
                        newline
                        write ")"
                in
                ifFitsOnOneLineOrElse horizontal vertical


instance Pretty InstRule where
    prettyInternal (IParen _ rule) = do
        write "("
        pretty rule
        write ")"
    prettyInternal (IRule _ mvarbinds mctx ihead) = do
        case mvarbinds of
            Nothing ->
                return ()

            Just xs -> do
                write "forall "
                spaced (map pretty xs)
                write ". "
        case mctx of
            Nothing ->
                pretty ihead

            Just ctx -> do
                mst <-
                    fitsOnOneLine
                        (do
                            pretty ctx
                            write " => "
                            pretty ihead
                            write " where"
                        )
                case mst of
                    Nothing ->
                        withCtx mctx (pretty ihead)

                    Just {} -> do
                        pretty ctx
                        write " => "
                        pretty ihead


instance Pretty InstHead where
    prettyInternal x =
        case
            x
            -- Base cases
        of
            IHCon _ name ->
                pretty name

            IHInfix _ typ' name ->
                depend (pretty typ')
                    (do
                        space
                        prettyInfixOp name
                    )
            -- Recursive application

            IHApp _ ihead typ' ->
                depend (pretty ihead)
                    (do
                        space
                        pretty typ'
                    )
            -- Wrapping in parens

            IHParen _ h -> do
                write "("
                pretty h
                write ")"


instance Pretty DeclHead where
    prettyInternal x =
        case x of
            DHead _ name ->
                prettyQuoteName name

            DHParen _ h -> do
                write "("
                pretty h
                write ")"

            DHInfix _ var name -> do
                pretty var
                space
                prettyInfixName name

            DHApp _ dhead var ->
                depend (pretty dhead)
                    (do
                        space
                        pretty var
                    )


instance Pretty Overlap where
    prettyInternal (Overlap _) =
        write "{-# OVERLAP #-}"
    prettyInternal (Overlapping _) =
        write "{-# OVERLAPPING #-}"
    prettyInternal (Overlaps _) =
        write "{-# OVERLAPS #-}"
    prettyInternal (Overlappable _) =
        write "{-# OVERLAPPABLE #-}"
    prettyInternal (NoOverlap _) =
        write "{-# NO_OVERLAP #-}"
    prettyInternal (Incoherent _) =
        write "{-# INCOHERENT #-}"


instance Pretty Sign where
    prettyInternal (Signless _) =
        return ()
    prettyInternal (Negative _) =
        write "-"


instance Pretty CallConv where
    prettyInternal =
        pretty'


instance Pretty Safety where
    prettyInternal =
        pretty'


--------------------------------------------------------------------------------
-- * Unimplemented or incomplete printers
instance Pretty Module where
    prettyInternal x =
        case x of
            Module _ mayModHead pragmas imps decls -> do
                inter twoEmptyLines
                    (mapMaybe
                        (\( isNull, r ) ->
                            if isNull then
                                Nothing

                            else
                                Just r
                        )
                        [ ( null pragmas, inter newline (map pretty pragmas) )
                        , (case mayModHead of
                            Nothing ->
                                ( True, return () )

                            Just modHead ->
                                ( False, pretty modHead )
                        )
                        , ( null imps, formatImports imps )
                        , ( null decls
                        , interOf newline
                            (map
                                (\case
                                    r@TypeSig {} ->
                                        ( 1, pretty r )

                                    r@InlineSig {} ->
                                        ( 1, pretty r )

                                    r ->
                                        ( 3, pretty r )
                                )
                                decls
                            )
                        )
                        ]
                    )
                newline
                where interOf i (( c, p ) : ps) =
                        case ps of
                            [] ->
                                p

                            _ -> do
                                p
                                replicateM_ c i
                                interOf i ps
                      interOf _ [] =
                        return ()

            XmlPage {} ->
                error "FIXME: No implementation for XmlPage."

            XmlHybrid {} ->
                error "FIXME: No implementation for XmlHybrid."


-- | Format imports, preserving empty newlines between groups.
formatImports :: [ImportDecl NodeInfo] -> Printer ()
formatImports =
    sequence_ . intersperse oneEmptyLine . map formatImportGroup
        . groupAdjacentBy atNextLine
    where
        atNextLine import1 import2 =
            let
                end1 =
                    srcSpanEndLine (srcInfoSpan (nodeInfoSpan (ann import1)))

                start2 =
                    srcSpanStartLine (srcInfoSpan (nodeInfoSpan (ann import2)))
            in
            start2 - end1 <= 1

        formatImportGroup imps =
            imps
                |> sortImports
                |> map formatImport
                |> intersperse newline
                |> sequence_

        moduleVisibleName idecl =
            let
                ModuleName _ name =
                    importModule idecl
            in
            name

        formatImport =
            pretty

        sortImports imps =
            sortOn moduleVisibleName . map sortImportSpecsOnImport <|
                imps

        sortImportSpecsOnImport imp =
            imp { importSpecs = fmap sortImportSpecs (importSpecs imp) }

        sortImportSpecs (ImportSpecList l hiding specs) =
            ImportSpecList l hiding sortedSpecs
            where
                sortedSpecs =
                    sortBy importSpecCompare . map sortCNames <| specs

                sortCNames (IThingWith l2 name cNames) =
                    IThingWith l2 name . sortBy cNameCompare <| cNames
                sortCNames is =
                    is


groupAdjacentBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAdjacentBy _ [] =
    []
groupAdjacentBy adj items =
    xs : groupAdjacentBy adj rest
    where
        ( xs, rest ) =
            spanAdjacentBy adj items


spanAdjacentBy :: (a -> a -> Bool) -> [a] -> ([a], [a])
spanAdjacentBy _ [] =
    ( [], [] )
spanAdjacentBy _ [x] =
    ( [ x ], [] )
spanAdjacentBy adj (x : xs@(y : _))
    | adj x y =
        let
            ( xs', rest' ) =
                spanAdjacentBy adj xs
        in
        ( x : xs', rest' )
    | otherwise = ( [ x ], xs )


importSpecCompare :: ImportSpec l -> ImportSpec l -> Ordering
importSpecCompare (IAbs _ _ (Ident _ s1)) (IAbs _ _ (Ident _ s2)) =
    compare s1 s2
importSpecCompare (IAbs _ _ (Ident _ _)) (IAbs _ _ (Symbol _ _)) =
    GT
importSpecCompare (IAbs _ _ (Ident _ s1)) (IThingAll _ (Ident _ s2)) =
    compare s1 s2
importSpecCompare (IAbs _ _ (Ident _ _)) (IThingAll _ (Symbol _ _)) =
    GT
importSpecCompare (IAbs _ _ (Ident _ s1)) (IThingWith _ (Ident _ s2) _) =
    compare s1 s2
importSpecCompare (IAbs _ _ (Ident _ _)) (IThingWith _ (Symbol _ _) _) =
    GT
importSpecCompare (IAbs _ _ (Symbol _ _)) (IAbs _ _ (Ident _ _)) =
    LT
importSpecCompare (IAbs _ _ (Symbol _ s1)) (IAbs _ _ (Symbol _ s2)) =
    compare s1 s2
importSpecCompare (IAbs _ _ (Symbol _ _)) (IThingAll _ (Ident _ _)) =
    LT
importSpecCompare (IAbs _ _ (Symbol _ s1)) (IThingAll _ (Symbol _ s2)) =
    compare s1 s2
importSpecCompare (IAbs _ _ (Symbol _ _)) (IThingWith _ (Ident _ _) _) =
    LT
importSpecCompare (IAbs _ _ (Symbol _ s1)) (IThingWith _ (Symbol _ s2) _) =
    compare s1 s2
importSpecCompare (IAbs _ _ _) (IVar _ _) =
    LT
importSpecCompare (IThingAll _ (Ident _ s1)) (IAbs _ _ (Ident _ s2)) =
    compare s1 s2
importSpecCompare (IThingAll _ (Ident _ _)) (IAbs _ _ (Symbol _ _)) =
    GT
importSpecCompare (IThingAll _ (Ident _ s1)) (IThingAll _ (Ident _ s2)) =
    compare s1 s2
importSpecCompare (IThingAll _ (Ident _ _)) (IThingAll _ (Symbol _ _)) =
    GT
importSpecCompare (IThingAll _ (Ident _ s1)) (IThingWith _ (Ident _ s2) _) =
    compare s1 s2
importSpecCompare (IThingAll _ (Ident _ _)) (IThingWith _ (Symbol _ _) _) =
    GT
importSpecCompare (IThingAll _ (Symbol _ _)) (IAbs _ _ (Ident _ _)) =
    LT
importSpecCompare (IThingAll _ (Symbol _ s1)) (IAbs _ _ (Symbol _ s2)) =
    compare s1 s2
importSpecCompare (IThingAll _ (Symbol _ _)) (IThingAll _ (Ident _ _)) =
    LT
importSpecCompare (IThingAll _ (Symbol _ s1)) (IThingAll _ (Symbol _ s2)) =
    compare s1 s2
importSpecCompare (IThingAll _ (Symbol _ _)) (IThingWith _ (Ident _ _) _) =
    LT
importSpecCompare (IThingAll _ (Symbol _ s1)) (IThingWith _ (Symbol _ s2) _) =
    compare s1 s2
importSpecCompare (IThingAll _ _) (IVar _ _) =
    LT
importSpecCompare (IThingWith _ (Ident _ s1) _) (IAbs _ _ (Ident _ s2)) =
    compare s1 s2
importSpecCompare (IThingWith _ (Ident _ _) _) (IAbs _ _ (Symbol _ _)) =
    GT
importSpecCompare (IThingWith _ (Ident _ s1) _) (IThingAll _ (Ident _ s2)) =
    compare s1 s2
importSpecCompare (IThingWith _ (Ident _ _) _) (IThingAll _ (Symbol _ _)) =
    GT
importSpecCompare (IThingWith _ (Ident _ s1) _) (IThingWith _ (Ident _ s2) _) =
    compare s1 s2
importSpecCompare (IThingWith _ (Ident _ _) _) (IThingWith _ (Symbol _ _) _) =
    GT
importSpecCompare (IThingWith _ (Symbol _ _) _) (IAbs _ _ (Ident _ _)) =
    LT
importSpecCompare (IThingWith _ (Symbol _ s1) _) (IAbs _ _ (Symbol _ s2)) =
    compare s1 s2
importSpecCompare (IThingWith _ (Symbol _ _) _) (IThingAll _ (Ident _ _)) =
    LT
importSpecCompare (IThingWith _ (Symbol _ s1) _) (IThingAll _ (Symbol _ s2)) =
    compare s1 s2
importSpecCompare (IThingWith _ (Symbol _ _) _) (IThingWith _ (Ident _ _) _) =
    LT
importSpecCompare (IThingWith _ (Symbol _ s1) _) (IThingWith _ (Symbol _ s2) _) =
    compare s1 s2
importSpecCompare (IThingWith _ _ _) (IVar _ _) =
    LT
importSpecCompare (IVar _ (Ident _ s1)) (IVar _ (Ident _ s2)) =
    compare s1 s2
importSpecCompare (IVar _ (Ident _ _)) (IVar _ (Symbol _ _)) =
    GT
importSpecCompare (IVar _ (Symbol _ _)) (IVar _ (Ident _ _)) =
    LT
importSpecCompare (IVar _ (Symbol _ s1)) (IVar _ (Symbol _ s2)) =
    compare s1 s2
importSpecCompare (IVar _ _) _ =
    GT


cNameCompare :: CName l -> CName l -> Ordering
cNameCompare (VarName _ (Ident _ s1)) (VarName _ (Ident _ s2)) =
    compare s1 s2
cNameCompare (VarName _ (Ident _ _)) (VarName _ (Symbol _ _)) =
    GT
cNameCompare (VarName _ (Ident _ s1)) (ConName _ (Ident _ s2)) =
    compare s1 s2
cNameCompare (VarName _ (Ident _ _)) (ConName _ (Symbol _ _)) =
    GT
cNameCompare (VarName _ (Symbol _ _)) (VarName _ (Ident _ _)) =
    LT
cNameCompare (VarName _ (Symbol _ s1)) (VarName _ (Symbol _ s2)) =
    compare s1 s2
cNameCompare (VarName _ (Symbol _ _)) (ConName _ (Ident _ _)) =
    LT
cNameCompare (VarName _ (Symbol _ s1)) (ConName _ (Symbol _ s2)) =
    compare s1 s2
cNameCompare (ConName _ (Ident _ s1)) (VarName _ (Ident _ s2)) =
    compare s1 s2
cNameCompare (ConName _ (Ident _ _)) (VarName _ (Symbol _ _)) =
    GT
cNameCompare (ConName _ (Ident _ s1)) (ConName _ (Ident _ s2)) =
    compare s1 s2
cNameCompare (ConName _ (Ident _ _)) (ConName _ (Symbol _ _)) =
    GT
cNameCompare (ConName _ (Symbol _ _)) (VarName _ (Ident _ _)) =
    LT
cNameCompare (ConName _ (Symbol _ s1)) (VarName _ (Symbol _ s2)) =
    compare s1 s2
cNameCompare (ConName _ (Symbol _ _)) (ConName _ (Ident _ _)) =
    LT
cNameCompare (ConName _ (Symbol _ s1)) (ConName _ (Symbol _ s2)) =
    compare s1 s2


instance Pretty Bracket where
    prettyInternal x =
        case x of
            ExpBracket _ p ->
                quotation "" (pretty p)

            PatBracket _ p ->
                quotation "p" (pretty p)

            TypeBracket _ ty ->
                quotation "t" (pretty ty)

            d@(DeclBracket _ _) ->
                pretty' d


instance Pretty IPBind where
    prettyInternal x =
        case x of
            IPBind _ name expr -> do
                pretty name
                space
                write "="
                space
                pretty expr


instance Pretty BooleanFormula where
    prettyInternal (VarFormula _ i@(Ident _ _)) =
        pretty' i
    prettyInternal (VarFormula _ (Symbol _ s)) =
        write "(" >> string s >> write ")"
    prettyInternal (AndFormula _ fs) = do
        maybeFormulas <- fitsOnOneLine <| inter (write ", ") <| map pretty fs
        case maybeFormulas of
            Nothing ->
                prefixedLined ", " (map pretty fs)

            Just formulas ->
                put formulas
    prettyInternal (OrFormula _ fs) = do
        maybeFormulas <- fitsOnOneLine <| inter (write " | ") <| map pretty fs
        case maybeFormulas of
            Nothing ->
                prefixedLined "| " (map pretty fs)

            Just formulas ->
                put formulas
    prettyInternal (ParenFormula _ f) = do
        write "("
        pretty f
        write ")"


--------------------------------------------------------------------------------
-- * Fallback printers
instance Pretty DataOrNew where
    prettyInternal =
        pretty'


instance Pretty FunDep where
    prettyInternal =
        pretty'


instance Pretty ResultSig where
    prettyInternal (KindSig _ kind) =
        pretty kind
    prettyInternal (TyVarSig _ tyVarBind) =
        pretty tyVarBind


instance Pretty Literal where
    prettyInternal (String _ _ rep) = do
        write "\""
        string rep
        write "\""
    prettyInternal (Char _ _ rep) = do
        write "'"
        string rep
        write "'"
    prettyInternal (PrimString _ _ rep) = do
        write "\""
        string rep
        write "\"#"
    prettyInternal (PrimChar _ _ rep) = do
        write "'"
        string rep
        write "'#"
    -- We print the original notation (because HSE doesn't track Hex
    -- vs binary vs decimal notation).
    prettyInternal (Int _l _i originalString) =
        string originalString
    prettyInternal (Frac _l _r originalString) =
        string originalString
    prettyInternal x =
        pretty' x


instance Pretty Name where
    prettyInternal x =
        case x of
            Ident _ _ ->
                pretty' x -- Identifiers.

            Symbol _ s ->
                string s -- Symbols


instance Pretty QName where
    prettyInternal =
        \case
            Qual _ mn n ->
                case n of
                    Ident _ i -> do
                        pretty mn
                        write "."
                        string i

                    Symbol _ s -> do
                        write "("
                        pretty mn
                        write "."
                        string s
                        write ")"

            UnQual _ n ->
                case n of
                    Ident _ i ->
                        string i

                    Symbol _ s -> do
                        write "("
                        string s
                        write ")"

            Special _ s@Cons {} -> do
                write "("
                pretty s
                write ")"

            Special _ s@FunCon {} -> do
                write "("
                pretty s
                write ")"

            Special _ s ->
                pretty s


instance Pretty SpecialCon where
    prettyInternal s =
        case s of
            UnitCon _ ->
                emptyParens

            ListCon _ ->
                emptyBrackets

            FunCon _ ->
                rightArrow

            TupleCon _ boxed i ->
                let
                    ( open, close ) =
                        boxWrap boxed
                in do
                    write open
                    space
                    write <| replicate (i - 1) ','
                    space
                    write close

            Cons _ -> do
                space
                write ":"
                space

            UnboxedSingleCon _ ->
                write "(##)"

            ExprHole _ ->
                write "_"


instance Pretty QOp where
    prettyInternal =
        pretty'


instance Pretty TyVarBind where
    prettyInternal =
        pretty'


instance Pretty ModuleHead where
    prettyInternal (ModuleHead _ name mwarnings mexports) = do
        write "module "
        pretty name
        maybe (return ()) pretty mwarnings
        maybe (return ())
            (\exports -> do
                newline
                indentedBlock (pretty exports)
            )
            mexports
        write " where"


instance Pretty ModulePragma where
    prettyInternal =
        pretty'


instance Pretty ImportDecl where
    prettyInternal (ImportDecl _ name qualified source safe mpkg mas mspec) = do
        write "import"
        when source <| write " {-# SOURCE #-}"
        when safe <| write " safe"
        when qualified <| write " qualified"
        case mpkg of
            Nothing ->
                return ()

            Just pkg ->
                space >> write ("\"" ++ pkg ++ "\"")
        space
        pretty name
        case mas of
            Nothing ->
                return ()

            Just asName -> do
                let asExpression = do
                        space
                        write "as"
                        space
                        pretty asName
                fitsOnLine <- fitsOnOneLine_ asExpression
                if fitsOnLine then
                    asExpression

                else do
                    newline
                    indentedBlock <| do
                        write "as"
                        space
                        pretty asName
        case mspec of
            Nothing ->
                return ()

            Just spec ->
                pretty spec


instance Pretty ModuleName where
    prettyInternal (ModuleName _ name) =
        write name


instance Pretty ImportSpecList where
    prettyInternal (ImportSpecList _ hiding spec) =
        let
            horizontal = do
                write "("
                spec
                    |> map pretty
                    |> commas
                write ")"

            vertical = do
                write "("
                space
                spec
                    |> setPrefixTail ", "
                    |> map pretty
                    |> lined
                newline
                write ")"
        in do
            when hiding <| do
                space
                write "hiding"
            space
            case spec of
                [] ->
                    emptyParens

                _ ->
                    ifFitsOnOneLineOrElse horizontal vertical


instance Pretty ImportSpec where
    prettyInternal =
        pretty'


instance Pretty WarningText where
    prettyInternal (DeprText _ s) =
        write "{-# DEPRECATED " >> string s >> write " #-}"
    prettyInternal (WarnText _ s) =
        write "{-# WARNING " >> string s >> write " #-}"


instance Pretty ExportSpecList where
    prettyInternal (ExportSpecList _ es) =
        let
            exportName x =
                x
                    |> fmap nodeInfoSpan
                    |> P.prettyPrint

            lowerExports =
                es
                    |> filter (not . isPrefixOf "module" . exportName)
                    |> filter (isLower . head . exportName)
                    |> sortOn exportName

            upperExports =
                es
                    |> filter (not . isPrefixOf "module" . exportName)
                    |> filter (isUpper . head . exportName)
                    |> sortOn exportName

            symbolExports =
                es
                    |> filter (not . isPrefixOf "module" . exportName)
                    |> filter ((==) '(' . head . exportName)
                    |> sortOn exportName

            moduleExports =
                es
                    |> filter (isPrefixOf "module" . exportName)
                    |> sortOn exportName

            sortedExports =
                moduleExports ++ upperExports ++ symbolExports ++ lowerExports
        in do
            write "("
            space
            sortedExports
                |> setPrefixTail ", "
                |> map pretty
                |> lined
            newline
            write ")"


instance Pretty ExportSpec where
    prettyInternal x =
        pretty' x


-- Do statements need to handle infix expression indentation specially because
-- do x *
--    y
-- is two invalid statements, not one valid infix op.
stmt :: Stmt NodeInfo -> Printer ()
stmt x =
    case x of
        Generator _ p e ->
            let
                leftSide = do
                    pretty p
                    space
                    write "<-"

                rightSide =
                    pretty e
            in do
                swing leftSide rightSide

        Qualifier _ e ->
            pretty e

        LetStmt _ binds ->
            withLetStatementContext True <| do
                writeLet
                space
                indentedBlock <| pretty binds

        RecStmt _ es ->
            depend (write "rec ") (lined (map pretty es))


-- | Handle do and case specially and also space out guards more.
rhs :: Rhs NodeInfo -> Printer ()
rhs (UnGuardedRhs _ e) =
    pretty e
rhs (GuardedRhss _ gas) =
    let
        line p = do
            write "|"
            pretty p
    in
    gas
        |> map line
        |> lined


-- | Implement dangling right-hand-sides.
guardedRhs :: GuardedRhs NodeInfo -> Printer ()
-- | Handle do specially.
guardedRhs (GuardedRhs _ stmts (Do _ dos)) = do
    indented 1
        (do
            prefixedLined ","
                (map
                    (\p -> do
                        space
                        pretty p
                    )
                    stmts
                )
        )
    inCase <- gets psInsideCase
    write
        (if inCase then
            " -> "

         else
            " = "
        )
    swing writeDo (lined (map pretty dos))
guardedRhs (GuardedRhs _ stmts e) = do
    mst <- fitsOnOneLine printStmts
    case mst of
        Just st -> do
            put st
            mst' <-
                fitsOnOneLine
                    (do
                        write " "
                        rhsSeparator
                        write " "
                        pretty e
                    )
            case mst' of
                Just st' ->
                    put st'

                Nothing ->
                    swingIt

        Nothing -> do
            printStmts
            swingIt
    where
        printStmts =
            indented 1
                (do
                    prefixedLined ","
                        (map
                            (\p -> do
                                space
                                pretty p
                            )
                            stmts
                        )
                )

        swingIt =
            swing (write " " >> rhsSeparator) (pretty e)


match :: Match NodeInfo -> Printer ()
match (Match _ name pats rhs' mbinds) = do
    case name of
        Ident _ _ ->
            pretty name

        Symbol _ _ -> do
            write "("
            pretty name
            write ")"
    space
    pats
        |> map pretty
        |> spaced
        |> indentedBlock
    case rhs' of
        GuardedRhss _ _ -> do
            newline
            indentedBlock <| pretty rhs'

        UnGuardedRhs _ (Do _ _) -> do
            space
            write "="
            space
            pretty rhs'

        _ -> do
            space
            write "="
            newline
            indentedBlock <| pretty rhs'
    for_ mbinds bindingGroup
match (InfixMatch _ pat1 name pats rhs' mbinds) = do
    depend
        (do
            pretty pat1
            space
            prettyInfixName name
        )
        (do
            space
            spaced (map pretty pats)
        )
    withCaseContext False (pretty rhs')
    for_ mbinds bindingGroup


-- | Format contexts with spaces and commas between class constraints.
context :: Context NodeInfo -> Printer ()
context ctx =
    case ctx of
        CxSingle _ a ->
            pretty a

        CxTuple _ as -> do
            write "("
            space
            as
                |> setPrefixTail ", "
                |> map pretty
                |> lined
            newline
            write ")"

        CxEmpty _ ->
            emptyParens


typ :: Type NodeInfo -> Printer ()
typ (TyTuple _ boxed types) =
    let
        ( open, close ) =
            boxWrap boxed

        horizontal = do
            write open
            types
                |> map pretty
                |> commas
            write close

        vertical = do
            write open
            space
            types
                |> setPrefixTail ", "
                |> map pretty
                |> lined
            newline
            write close
    in
    case types of
        [] ->
            emptyParens

        _ ->
            ifFitsOnOneLineOrElse horizontal vertical
typ (TyForall _ mbinds ctx ty) =
    depend
        (case mbinds of
            Nothing ->
                return ()

            Just ts -> do
                write "forall "
                spaced (map pretty ts)
                write ". "
        )
        (do
            withCtx ctx (indentedBlock (pretty ty))
        )
typ (TyFun _ a b) =
    depend
        (do
            pretty a
            write " -> "
        )
        (pretty b)
typ (TyList _ t) = do
    write "["
    pretty t
    write "]"
typ (TyParArray _ t) = do
    write "["
    write ":"
    pretty t
    write ":"
    write "]"
typ (TyApp _ f a) =
    spaced [ pretty f, pretty a ]
typ (TyVar _ n) =
    pretty n
typ (TyCon _ p) =
    pretty p
typ (TyParen _ e) = do
    write "("
    pretty e
    write ")"
typ (TyInfix _ a promotedop b)
    -- Apply special rules to line-break operators.
 = do
    let symbolName = getSymbolNameTy promotedop
        prettyInfixOp' op =
            case op of
                PromotedName _ op' ->
                    write "'" >> prettyInfixOp op'

                UnpromotedName _ op' ->
                    prettyInfixOp op'
    linebreak <- isLineBreakBefore symbolName
    if linebreak then do
        pretty a
        newline
        prettyInfixOp' promotedop
        space
        pretty b

    else do
        pretty a
        space
        prettyInfixOp' promotedop
        space
        pretty b
typ (TyKind _ ty k) = do
    write "("
    pretty ty
    space
    write "::"
    space
    pretty k
    write ")"
typ (TyBang _ bangty unpackty right) = do
    pretty unpackty
    pretty bangty
    pretty right
typ (TyEquals _ left right) = do
    pretty left
    write " ~ "
    pretty right
typ (TyPromoted _ (PromotedList _ _ ts)) = do
    write "'["
    unless (null ts) <| write " "
    commas (map pretty ts)
    write "]"
typ (TyPromoted _ (PromotedTuple _ ts)) = do
    write "'("
    unless (null ts) <| write " "
    commas (map pretty ts)
    write ")"
typ (TyPromoted _ (PromotedCon _ _ tname)) = do
    write "'"
    pretty tname
typ (TyPromoted _ (PromotedString _ _ raw)) = do
    do
        write "\""
        string raw
        write "\""
typ ty@TyPromoted {} =
    pretty' ty
typ (TySplice _ splice) =
    pretty splice
typ (TyWildCard _ name) =
    case name of
        Nothing ->
            write "_"

        Just n -> do
            write "_"
            pretty n
typ (TyQuasiQuote _ n s) =
    quotation n (string s)
typ (TyUnboxedSum {}) =
    error "FIXME: No implementation for TyUnboxedSum."
typ (TyStar _) =
    write "*"


prettyTopName :: Name NodeInfo -> Printer ()
prettyTopName x@Ident {} =
    pretty x
prettyTopName x@Symbol {} = do
    write "("
    pretty x
    write ")"


-- | Specially format records. Indent where clauses only 2 spaces.
decl' :: Decl NodeInfo -> Printer ()
-- | Pretty print type signatures like
--
-- foo :: (Show x, Read x)
--     => (Foo -> Bar)
--     -> Maybe Int
--     -> (Char -> X -> Y)
--     -> IO ()
--
decl' (TypeSig _ names ty') =
    let
        firstPart = do
            names
                |> map prettyTopName
                |> commas
            space
            write "::"

        oneline = do
            firstPart
            space
            declTy False ty'

        multiline = do
            firstPart
            newline
            indentedBlock <| do
                declTy True ty'
    in do
        ifFitsOnOneLineOrElse oneline multiline
decl' (PatBind _ pat rhs' mbinds) =
    withCaseContext False <| do
        pretty pat
        case rhs' of
            GuardedRhss _ _ -> do
                newline
                indentedBlock <| pretty rhs'

            UnGuardedRhs _ (Do _ _) -> do
                space
                write "="
                space
                pretty rhs'

            _ -> do
                space
                write "="
                isInLetStatement <- gets psInsideLetStatement
                let expression = pretty rhs'
                isOneLine <- fitsOnOneLine_ expression
                if isInLetStatement && isOneLine then do
                    space
                    pretty rhs'

                else do
                    newline
                    indentedBlock <| pretty rhs'
        for_ mbinds bindingGroup
-- | Handle records specially for a prettier display (see guide).
decl' e =
    decl e


declTy :: Bool -> Type NodeInfo -> Printer ()
declTy breakLine dty =
    case dty of
        TyForall _ _ _ ty -> do
            prettyTyForall breakLine dty
            prettyTy breakLine ty

        _ ->
            prettyTy breakLine dty


prettyTyForall :: Bool -> Type NodeInfo -> Printer ()
prettyTyForall breakLine (TyForall _ mbinds mctx _) =
    let
        ifBreak breakPrinter noBreakPrinter =
            if breakLine then
                breakPrinter

            else
                noBreakPrinter

        writeMBinds =
            case mbinds of
                Just binds -> do
                    writeForall
                    binds
                        |> map pretty
                        |> spaced
                    write "."
                    ifBreak nothing space

                Nothing ->
                    nothing

        writeMCtx =
            case mctx of
                Just ctx -> do
                    pretty ctx
                    ifBreak newline space
                    rightFatArrow
                    space

                Nothing ->
                    nothing
    in do
        writeMBinds
        writeMCtx


prettyTy :: Bool -> Type NodeInfo -> Printer ()
prettyTy breakLine ty =
    let
        collapseFaps (TyFun _ arg result) =
            arg : collapseFaps result
        collapseFaps e =
            [ e ]

        oneline =
            pretty ty

        multiline =
            case collapseFaps ty of
                [] ->
                    pretty ty

                tys -> do
                    tys
                        |> setPrefixTail "-> "
                        |> map pretty
                        |> lined
    in do
        isOneLine <- fitsOnOneLine_ oneline
        if breakLine || (not isOneLine) then
            multiline

        else
            oneline


-- | Fields are preceded with a space.
conDecl :: ConDecl NodeInfo -> Printer ()
conDecl (RecDecl _ name fields) = do
    pretty name
    newline
    indentedBlock <| do
        write "{"
        space
        fields
            |> setPrefixTail ", "
            |> map pretty
            |> lined
        newline
        write "}"
conDecl (ConDecl _ name bangty) = do
    prettyQuoteName name
    unless (null bangty)
        (ifFitsOnOneLineOrElse
            (do
                space
                spaced (map pretty bangty)
            )
            (do
                newline
                indentedBlock (lined (map pretty bangty))
            )
        )
conDecl (InfixConDecl _ a f b) =
    inter space [ pretty a, pretty f, pretty b ]


recUpdateExpr ::
    Exp NodeInfo
    -> Printer ()
    -> [FieldUpdate NodeInfo]
    -> Printer ()
recUpdateExpr wholeExpression expWriter updates =
    let
        isBreakFromFile =
            srcSpanStartLine srcSpan /= srcSpanEndLine srcSpan

        srcSpan =
            ann wholeExpression
                |> nodeInfoSpan
                |> srcInfoSpan

        horizontal = do
            expWriter
            space
            write "{"
            space
            updates
                |> map pretty
                |> commas
            space
            write "}"

        vertical = do
            expWriter
            newline
            indentedBlock <| do
                write "{"
                space
                updates
                    |> setPrefixTail ", "
                    |> map pretty
                    |> lined
                newline
                write "}"
    in do
        case updates of
            [] -> do
                expWriter
                space
                emptyBraces

            _ ->
                if isBreakFromFile then
                    vertical

                else
                    ifFitsOnOneLineOrElse horizontal vertical


--------------------------------------------------------------------------------
getSymbolNameTy :: MaybePromotedName NodeInfo -> QName NodeInfo
getSymbolNameTy symbol =
    case symbol of
        PromotedName _ symbolName ->
            symbolName

        UnpromotedName _ symbolName ->
            symbolName


getSymbolNameOp :: QOp NodeInfo -> QName NodeInfo
getSymbolNameOp symbol =
    case symbol of
        QVarOp _ symbolName ->
            symbolName

        QConOp _ symbolName ->
            symbolName


-- Predicates
-- | If the given operator is an element of line breaks before in configuration.
isLineBreakBefore :: QName NodeInfo -> Printer Bool
isLineBreakBefore (UnQual _ (Symbol _ s)) = do
    breaks <- gets (configLineBreaksBefore . psConfig)
    let isInConfig = elem s breaks
    return isInConfig
isLineBreakBefore _ =
    return False


-- | If the given operator is an element of line breaks after in configuration.
isLineBreakAfter :: QName NodeInfo -> Printer Bool
isLineBreakAfter (UnQual _ (Symbol _ s)) = do
    breaks <- gets (configLineBreaksAfter . psConfig)
    let isInConfig = elem s breaks
    return isInConfig
isLineBreakAfter _ =
    return False


bindingGroup :: Binds NodeInfo -> Printer ()
bindingGroup binds = do
    newline
    indentedBlock <| do
        write "where"
        newline
        indentedBlock (pretty binds)


infixApp ::
    Exp NodeInfo
    -> Exp NodeInfo
    -> QOp NodeInfo
    -> Exp NodeInfo
    -> Printer ()
infixApp wholeExpression a op b =
    let
        horizontal = do
            pretty a
            space
            pretty op
            space
            pretty b

        verticalBefore =
            verticalInfixApplicationBefore a op b

        verticalAfter =
            verticalInfixApplicationAfter a op b

        isBreakFromFile =
            srcSpanStartLine srcSpan /= srcSpanEndLine srcSpan

        srcSpan =
            ann wholeExpression
                |> nodeInfoSpan
                |> srcInfoSpan

        bIsDo =
            case b of
                Do _ _ ->
                    True

                _ ->
                    False

        bIsCase =
            case b of
                Case _ _ _ ->
                    True

                _ ->
                    False
    in do
        let symbolName = getSymbolNameOp op
        isBreakBeforeFromConfig <- isLineBreakBefore symbolName
        isBreakAfterFromConfig <- isLineBreakAfter symbolName
        if bIsDo then
            horizontal

        else if bIsCase then
            verticalAfter

        else if isBreakBeforeFromConfig then
            verticalBefore

        else if isBreakFromFile && isBreakAfterFromConfig then
            verticalAfter

        else if isBreakAfterFromConfig then
            ifFitsOnOneLineOrElse horizontal verticalAfter

        else if isBreakFromFile then
            verticalBefore

        else
            ifFitsOnOneLineOrElse horizontal verticalBefore


verticalInfixApplicationBefore ::
    Exp NodeInfo
    -> QOp NodeInfo
    -> Exp NodeInfo
    -> Printer ()
verticalInfixApplicationBefore a op b =
    case b of
        App _ _ _ -> do
            pretty a
            newline
            indentedBlock <| do
                pretty op
                space
                pretty b

        _ -> do
            pretty a
            newline
            indentedBlock <| do
                pretty op
                isOneLine <- fitsOnOneLine_ (space >> pretty b)
                if isOneLine then do
                    space
                    pretty b

                else do
                    newline
                    indentedBlock <| pretty b


verticalInfixApplicationAfter ::
    Exp NodeInfo
    -> QOp NodeInfo
    -> Exp NodeInfo
    -> Printer ()
verticalInfixApplicationAfter a op b = do
    pretty a
    space
    pretty op
    newline
    indentedBlock (pretty b)

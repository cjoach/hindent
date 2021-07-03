{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | Haskell indenter.


module HIndent
    ( defaultExtensions
    , getExtensions
    , parseMode
    , prettyPrint
    , reformat
    , test
    , testAst
    , testFile
    , testFileAst
    ) where


-- * Formatting functions.


import CodeBlock
import Control.Monad.State.Strict
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Unsafe as S
import Data.Either
import Data.Function
import Data.Functor.Identity
import qualified Data.List as List
import Data.List ((\\))
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable hiding (mapM)
import qualified Language.Haskell.Exts as Exts
import Language.Haskell.Exts hiding (Pretty, Style, parse, prettyPrint, style)
import Prelude
import Pretty
import Types
import Utils.Flow


-- | Format the given source.


reformat ::
    Config
    -> Maybe [Extension]
    -> Maybe FilePath
    -> ByteString
    -> Either String Builder
reformat config mexts mfilepath =
    fmap (\x -> x <> "\n") . fmap (mconcat . List.intersperse "\n")
        . mapM processBlock
        . cppSplitBlocks
    where
        processBlock :: CodeBlock -> Either String Builder
        processBlock (Shebang text) =
            Right <| S.byteString text

        processBlock (CPPDirectives text) =
            Right <| S.byteString text

        processBlock (HaskellSource line text) =
            let
                ls =
                    S8.lines text

                prefix =
                    findPrefix ls

                code =
                    unlines' (map (stripPrefix prefix) ls)

                exts =
                    readExtensions (UTF8.toString code)

                mode'' =
                    case exts of
                        Nothing ->
                            mode'

                        Just ( Nothing, exts' ) ->
                            mode'
                                { extensions =
                                    exts' ++ configExtensions config
                                        ++ extensions mode'
                                }

                        Just ( Just lang, exts' ) ->
                            mode'
                                { baseLanguage = lang
                                , extensions =
                                    exts' ++ configExtensions config
                                        ++ extensions mode'
                                }
            in
            case parseModuleWithComments mode'' (UTF8.toString code) of
                ParseOk ( m, comments ) ->
                    fmap
                        (S.lazyByteString . addPrefix prefix
                            . S.toLazyByteString
                        )
                        (prettyPrint config m comments)

                ParseFailed loc' e ->
                    Left
                        (Exts.prettyPrint
                            (loc' { srcLine = srcLine loc' + line })
                            ++ ": "
                            ++ e
                        )

        unlines' =
            S.concat . List.intersperse "\n"

        unlines'' =
            L.concat . List.intersperse "\n"

        addPrefix :: ByteString -> L8.ByteString -> L8.ByteString
        addPrefix prefix =
            unlines'' . map (L8.fromStrict prefix <>) . L8.lines

        stripPrefix :: ByteString -> ByteString -> ByteString
        stripPrefix prefix line =
            if S.null (S8.dropWhile (== '\n') line) then
                line

            else
                fromMaybe (error "Missing expected prefix")
                    . s8_stripPrefix prefix <|
                    line

        findPrefix :: [ByteString] -> ByteString
        findPrefix =
            takePrefix False . findSmallestPrefix . dropNewlines

        dropNewlines :: [ByteString] -> [ByteString]
        dropNewlines =
            filter (not . S.null . S8.dropWhile (== '\n'))

        takePrefix :: Bool -> ByteString -> ByteString
        takePrefix bracketUsed txt =
            case S8.uncons txt of
                Nothing ->
                    ""

                Just ( '>', txt' ) ->
                    if not bracketUsed then
                        S8.cons '>' (takePrefix True txt')

                    else
                        ""

                Just ( c, txt' ) ->
                    if c == ' ' || c == '\t' then
                        S8.cons c (takePrefix bracketUsed txt')

                    else
                        ""

        findSmallestPrefix :: [ByteString] -> ByteString
        findSmallestPrefix [] =
            ""

        findSmallestPrefix ("" : _) =
            ""

        findSmallestPrefix (p : ps) =
            let
                first =
                    S8.head p

                startsWithChar c x =
                    S8.length x > 0 && S8.head x == c
            in
            if all (startsWithChar first) ps then
                S8.cons first (findSmallestPrefix (S.tail p : map S.tail ps))

            else
                ""

        mode' =
            let
                m =
                    case mexts of
                        Just exts ->
                            parseMode { extensions = exts }

                        Nothing ->
                            parseMode
            in
            m { parseFilename = fromMaybe "<interactive>" mfilepath }


-- | Print the module.


prettyPrint :: Config -> Module SrcSpanInfo -> [Comment] -> Either a Builder
prettyPrint config m comments =
    let
        ast =
            evalState (collectAllComments m) comments
    in
    Right (runPrinterStyle config (pretty ast))


-- | Pretty print the given printable thing.


runPrinterStyle :: Config -> Printer () -> Builder
runPrinterStyle config m =
    let
        printState =
            PrintState
                { psIndentLevel = 0
                , psColumnStart = 0
                , psOutput = mempty
                , psNewline = False
                , psColumn = 0
                , psLine = 1
                , psConfig = config
                , psInsideCase = False
                , psInsideLetStatement = False
                , psFitOnOneLine = False
                , psEolComment = False
                }
    in
    printState
        |> execStateT (runPrinter m)
        |> runMaybeT
        |> runIdentity
        |> maybe (error "Printer failed with mzero call.") psOutput


-- | Parse mode, includes all extensions, doesn't assume any fixities.


parseMode :: ParseMode
parseMode =
    defaultParseMode { extensions = allExtensions, fixities = Nothing }
    where
        allExtensions =
            filter isDisabledExtension knownExtensions

        isDisabledExtension (DisableExtension _) =
            False

        isDisabledExtension _ =
            True


-- | Test the given file.


testFile :: FilePath -> IO ()
testFile fp =
    S.readFile fp >>= test


-- | Test the given file.


testFileAst :: FilePath -> IO ()
testFileAst fp =
    S.readFile fp >>= print . testAst


-- | Test with the given style, prints to stdout.


test :: ByteString -> IO ()
test =
    either error (L8.putStrLn . S.toLazyByteString)
        . reformat defaultConfig Nothing Nothing


-- | Parse the source and annotate it with comments, yielding the resulting AST.


testAst :: ByteString -> Either String (Module NodeInfo)
testAst x =
    case parseModuleWithComments parseMode (UTF8.toString x) of
        ParseOk ( m, comments ) ->
            Right
                (let
                    ast =
                        evalState (collectAllComments m) comments
                 in
                 ast
                )

        ParseFailed _ e ->
            Left e


-- | Default extensions.


defaultExtensions :: [Extension]
defaultExtensions =
    [ e | e@EnableExtension {} <- knownExtensions ]
        \\ map EnableExtension badExtensions


-- | Extensions which steal too much syntax.


badExtensions :: [KnownExtension]
badExtensions =
    [ Arrows -- steals proc
    , TransformListComp -- steals the group keyword
    , XmlSyntax
    , RegularPatterns -- steals a-b
    , UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    , PatternSynonyms -- steals the pattern keyword
    , RecursiveDo -- steals the rec keyword
    , DoRec -- same
    , TypeApplications -- since GHC 8 and haskell-src-exts-1.19
    ]


s8_stripPrefix :: ByteString -> ByteString -> Maybe ByteString
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
    | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
    | otherwise = Nothing


--------------------------------------------------------------------------------
-- Extensions stuff stolen from hlint
-- | Consume an extensions list from arguments.


getExtensions :: [Text] -> [Extension]
getExtensions =
    List.foldl' f defaultExtensions . map T.unpack
    where
        f _ "Haskell98" =
            []

        f a ('N' : 'o' : x)
            | Just x' <- readExtension x = List.delete x' a

        f a x
            | Just x' <- readExtension x = x' : List.delete x' a

        f _ x =
            error <| "Unknown extension: " ++ x


--------------------------------------------------------------------------------
-- Comments
-- | Traverse the structure backwards.


traverseInOrder ::
    (Monad m, Traversable t, Functor m)
    => (b -> b -> Ordering)
    -> (b -> m b)
    -> t b
    -> m (t b)
traverseInOrder cmp f ast = do
    indexed <-
        fmap
            (zip [ 0 :: Integer .. ] . reverse)
            (execStateT (traverse (modify . ( : )) ast) [])
    let sorted = List.sortBy (\( _, x ) ( _, y ) -> cmp x y) indexed
    results <-
        mapM
            (\( i, m ) -> do
                v <- f m
                return ( i, v )
            )
            sorted
    evalStateT
        (traverse
            (const
                (do
                    i <- gets head
                    modify tail
                    case lookup i results of
                        Nothing ->
                            error "traverseInOrder"

                        Just x ->
                            return x
                )
            )
            ast
        )
        [ 0 .. ]


-- | Collect all comments in the module by traversing the tree. Read
-- this from bottom to top.


collectAllComments :: Module SrcSpanInfo -> State [Comment] (Module NodeInfo)
collectAllComments =
    let
        nodify s =
            NodeInfo s mempty ""

        -- Sort the comments by their end position.
        traverseBackwards =
            traverseInOrder
                (\x y ->
                    on
                        (flip compare)
                        (srcSpanEnd . srcInfoSpan . nodeInfoSpan)
                        x
                        y -- Stop traversing if all comments have been consumed.
                )

        shortCircuit m v = do
            comments <- get
            if null comments then
                return v

            else
                m v

        atFirstColumn commentSpan nodeSpan =
            (srcSpanStartColumn commentSpan == 1)
                && (srcSpanStartColumn nodeSpan == 1)

        commentBeforeNode commentSpan nodeSpan =
            srcSpanStartLine commentSpan < srcSpanStartLine nodeSpan

        commentAfterNode commentSpan nodeSpan =
            srcSpanStartLine commentSpan > srcSpanStartLine nodeSpan
    in
    shortCircuit
        (traverse
            -- Finally, collect forward comments which come before each node.
            (collectCommentsBy
                CommentBeforeLine
                (\nodeSpan commentSpan ->
                    srcSpanEndLine commentSpan < srcSpanStartLine nodeSpan
                )
            )
        )
        <=< shortCircuit
            (traverse
                -- Collect forwards comments which start at the end line of a
                -- node: Does the start line of the comment match the end-line
                -- of the node?
                (collectCommentsBy
                    CommentSameLine
                    (\nodeSpan commentSpan ->
                        srcSpanStartLine commentSpan == srcSpanEndLine nodeSpan
                    )
                )
            )
        <=< shortCircuit
            (traverseBackwards
                -- Collect backwards comments which are on the same line as a
                -- node: Does the start line & end line of the comment match
                -- that of the node?
                (collectCommentsBy
                    CommentSameLine
                    (\nodeSpan commentSpan ->
                        (srcSpanStartLine commentSpan
                            == srcSpanStartLine nodeSpan
                        )
                            &&
                                (srcSpanStartLine commentSpan
                                    == srcSpanEndLine nodeSpan
                                )
                    )
                )
            )
        <=< shortCircuit
            (traverseBackwards
                -- First, collect forwards comments for declarations which both
                -- start on column 1 and occur before the declaration.
                (collectCommentsBy
                    TopLevelCommentAfterLine
                    (\nodeSpan commentSpan ->
                        atFirstColumn commentSpan nodeSpan
                            && commentAfterNode commentSpan nodeSpan
                    )
                )
            )
        <=< shortCircuit
            (traverse
                -- First, collect forwards comments for declarations which both
                -- start on column 1 and occur before the declaration.
                (collectCommentsBy
                    TopLevelCommentBeforeLine
                    (\nodeSpan commentSpan ->
                        atFirstColumn commentSpan nodeSpan
                            && commentBeforeNode commentSpan nodeSpan
                    )
                )
            )
        . fmap nodify


-- | Collect comments by satisfying the given predicate, to collect a
-- comment means to remove it from the pool of available comments in
-- the State. This allows for a multiple pass approach.


collectCommentsBy ::
    (SrcSpan -> SomeComment -> NodeComment)
    -> (SrcSpan -> SrcSpan -> Bool)
    -> NodeInfo
    -> State [Comment] NodeInfo
collectCommentsBy cons predicate nodeInfo@(NodeInfo (SrcSpanInfo nodeSpan _) _ _) = do
    comments <- get
    let ( others, mine ) =
            partitionEithers
                (map
                    (\comment@(Comment _ commentSpan _) ->
                        if predicate nodeSpan commentSpan then
                            Right comment

                        else
                            Left comment
                    )
                    comments
                )
    put others
    return <| addCommentsToNode cons mine nodeInfo


-- | Reintroduce comments which were immediately above declarations in where clauses.
-- Affects where clauses of top level declarations only.


addCommentsToNode ::
    (SrcSpan -> SomeComment -> NodeComment)
    -> [Comment]
    -> NodeInfo
    -> NodeInfo
addCommentsToNode mkNodeComment newComments nodeInfo@(NodeInfo (SrcSpanInfo _ _) existingComments _) =
    let
        mkBeforeNodeComment :: Comment -> NodeComment
        mkBeforeNodeComment (Comment multiLine commentSpan commentString) =
            let
                commentType =
                    if multiLine then
                        MultiLine

                    else
                        EndOfLine
            in
            commentString
                |> T.pack
                |> T.stripEnd
                |> T.unpack
                |> commentType
                |> mkNodeComment commentSpan

        allComments =
            newComments
                |> map mkBeforeNodeComment
                |> (\c -> existingComments <> c)
    in
    nodeInfo { nodeInfoComments = allComments }

-- | Main entry point to hindent.
--
-- hindent


module Main
    ( main
    ) where


import CabalFile
import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Version (showVersion)
import qualified Data.Yaml as Y
import Find (findFileUp)
import Foreign.C.Error
import GHC.IO.Exception
import HIndent
import Language.Haskell.Exts hiding (Style, style)
import Options.Applicative hiding (action, style)
import Path
import qualified Path.IO as Path
import Paths_hindent_elm (version)
import qualified System.Directory as IO
import System.Exit (exitWith)
import qualified System.IO as IO
import Types
import Utils.Flow

-- import           Data.Monoid ((<>))


import qualified Data.Text as T


data Action
    = Validate
    | Reformat


data RunMode
    = ShowVersion
    | Run Config [Extension] Action [FilePath]


-- | Main entry point.


main :: IO ()
main = do
    config <- getConfig
    runMode <-
        execParser
            ( info
                (options config <**> helper)
                (header "hindent-elm - Reformat Haskell source code")
            )
    case runMode of
        ShowVersion ->
            putStrLn ("hindent-elm " ++ showVersion version)

        Run style exts action paths ->
            if null paths then
                L8.interact
                    ( either error S.toLazyByteString
                        . reformat style (Just exts) Nothing
                        . L8.toStrict
                    )

            else
                forM_ paths <|
                    \filepath ->
                        let
                            fn cabalexts' text' =
                                case
                                    reformat
                                        style
                                        (Just <| cabalexts' ++ exts)
                                        (Just filepath)
                                        text'
                                of
                                    Left e ->
                                        error e

                                    Right out ->
                                        unless
                                            ( L8.fromStrict text'
                                                == S.toLazyByteString out
                                            ) <|
                                            case action of
                                                Validate -> do
                                                    IO.putStrLn <|
                                                        filepath
                                                        ++ " is not formatted"
                                                    exitWith (ExitFailure 1)

                                                Reformat -> do
                                                    tmpDir <-
                                                        IO.getTemporaryDirectory
                                                    ( fp, h ) <-
                                                        IO.openTempFile
                                                            tmpDir
                                                            "hindent.hs"
                                                    L8.hPutStr
                                                        h
                                                        (S.toLazyByteString out)
                                                    IO.hFlush h
                                                    IO.hClose h
                                                    let exdev e =
                                                            if
                                                                ioe_errno e
                                                                    == Just
                                                                        ( ( \(Errno a) ->
                                                                                a
                                                                          )
                                                                            eXDEV
                                                                        )
                                                            then
                                                                IO.copyFile
                                                                    fp
                                                                    filepath
                                                                    >> IO.removeFile
                                                                        fp

                                                            else
                                                                throw e
                                                    IO.copyPermissions
                                                        filepath
                                                        fp
                                                    IO.renameFile fp filepath
                                                        `catch` exdev
                        in do
                            cabalexts <-
                                getCabalExtensionsForSourcePath filepath
                            text <- S.readFile filepath
                            fn cabalexts text


-- | Read config from a config file, or return 'defaultConfig'.


getConfig :: IO Config
getConfig = do
    cur <- Path.getCurrentDir
    homeDir <- Path.getHomeDir
    mfile <-
        findFileUp
            cur
            ((== ".hindent-elm.yaml") . toFilePath . filename)
            (Just homeDir)
    case mfile of
        Nothing ->
            return defaultConfig

        Just file -> do
            result <- Y.decodeFileEither (toFilePath file)
            case result of
                Left e ->
                    error (show e)

                Right config ->
                    return config


-- | Program options.


options :: Config -> Parser RunMode
options config =
    flag' ShowVersion (long "version" <> help "Print the version")
        <|> (Run <$> style <*> exts <*> action <*> files)
    where
        style =
            (makeStyle config <$> lineLen <*> indentSpaces)
                <* optional
                    ( strOption
                        ( long "style"
                            <> help
                                "Style to print with (historical, now ignored)"
                            <> metavar "STYLE"
                        ) :: Parser String
                    )

        exts =
            fmap
                getExtensions
                ( many
                    ( T.pack
                        <$> strOption
                            ( short 'X' <> help "Language extension"
                                <> metavar "GHCEXT"
                            )
                    )
                )

        indentSpaces =
            option
                auto
                ( long "indent-size" <> help "Indentation size in spaces"
                    <> value (configIndentSpaces config)
                    <> showDefault
                )
                <|> option
                    auto
                    ( long "tab-size"
                        <> help "Same as --indent-size, for compatibility"
                    )

        lineLen =
            option
                auto
                ( long "line-length" <> help "Desired length of lines"
                    <> value (configMaxColumns config)
                    <> showDefault
                )

        action =
            flag
                Reformat
                Validate
                ( long "validate"
                    <> help "Check if files are formatted without changing them"
                )

        makeStyle s mlen tabs =
            s
                { configMaxColumns = mlen
                , configIndentSpaces = tabs
                }

        files =
            many (strArgument (metavar "FILENAMES"))

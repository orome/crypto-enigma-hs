import Options.Applicative      -- http://hackage.haskell.org/package/optparse-applicative
--import Data.Maybe
import Data.Monoid ((<>))       -- For GHC 8.0 through 8.2
--import Control.Exception (catch)

--import Crypto.Enigma.Utils
import Crypto.Enigma
import Crypto.Enigma.Display


cliName = "Enigma machine CLI"

data Subcommand =
        Encode { config :: String, message :: String } |
        Show { config :: String, letterO :: Maybe String, formatO :: Maybe String, highlight0 :: Maybe String, encoding0 :: Maybe Bool } |
        Run { config :: String, messageO :: Maybe String, formatO :: Maybe String }
        deriving Show

data Options = Options { subcommand :: Subcommand } deriving Show

commandO = Options <$> subcommandO
--commandO = Options <$> generalOpts <*> subcommandO
-- generalOpts = strOption ( long "general" <> help "General option" )

subcommandO :: Parser Subcommand
subcommandO =
  subparser (
        command "encode" (info (Encode <$> configArg <*> messageArg <**> helper)
                         (helpText "Encode a message" "ENCODE" "encode foot")) <>
        command "show"   (info (Show <$> configArg <*> letterOpt <*> formatOpt <*> highlightOpt <*> encodingOpt <**> helper)
                         (helpText "Show a machine configuration" "SHOW" "show foot")) <>
        command "run"    (info (Run <$> configArg <*> messageOpt <*> formatOpt <**> helper)
                         (helpText "Run a machine " "Run" "run foot"))
   )
  where
        configArg = strArgument $ metavar "CONFIG" <>
                help "Config of machine"
        messageArg = strArgument $ metavar "MESSAGE" <> help "The message"
        messageOpt = optional $ strOption ( long "message" <> short 'm' <> metavar "MESSAGE" <> value " " <>
                help "The message OPT")
        letterOpt =  optional $ strOption ( long "letter" <> short 'l' <> metavar "LETTER" <> value " " <>
                help "Letter to highlight")
        formatOpt =  optional $ strOption ( long "format" <> short 'f' <> metavar "FORMAT" <> value "single" <>
                help "The format")
        highlightOpt =  optional $ strOption ( long "highlight" <> short 'H' <> metavar "HH" <> value "bars" <>
                help "The highlight")
        encodingOpt =  optional $ switch ( long "showencoding" <> short 'e' <>
                help "Show encoding")

        helpText desc cmd foot = (progDesc desc <>
                header (cliName ++ ": "++ cmd ++" command") <>
                footer ("Shared footer. " ++ foot))


main :: IO ()
main = do
--  (opts :: Options) <- execParser optsParser
    opts <- execParser optsParser
    case subcommand opts of
        Encode config message -> putStr $ showEnigmaEncoding (read config :: EnigmaConfig) message
        Show config (Just (letter:_)) (Just format) (Just highlight) (Just showenc) -> putStrLn $ displayEnigmaConfig (read config :: EnigmaConfig) letter format showenc (decorate highlight)
        Run config (Just message) (Just "single")-> putStrLn $ showEnigmaOperation (read config :: EnigmaConfig) message
        Run config (Just message) (Just "internal") -> putStrLn $ showEnigmaOperationInternal (read config :: EnigmaConfig) message
  where
    optsParser :: ParserInfo Options
    optsParser =
        info (helper <*> commandO)
             (  fullDesc <>
                progDesc "Command line interface to crypto-enigma package" <>
                header cliName <>
                footer "Some footer info")
    -- TBD -- Rename <<<
    -- decorate :: String -> (Char -> String)
    decorate spec = case spec of
                            "bars" -> \ch -> ch:"\818\773"
                            -- TBD - Colors
                            [l, r] -> \c -> [l, c, r]
                            _ -> \c -> [c]
    --decorate ch = ['[',ch,']'] -- version that works when Unicode fails to display properly (e.g. IHaskell as of 0.7.1.0)




-- stack exec -- enigma encode "c-β-V-VI-VIII CDTJ AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW 05.16.05.12" "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
-- RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ

-- stack exec -- enigma show "c-β-V-VI-VIII CDTJ AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW 05.16.05.12" -l 'G'
-- G > XKINZGF̲̅SCQBMLDVTJYHPWOUARE  CDTJ  25 15 16 25

-- stack exec -- enigma show "c-β-V-VI-VIII CDTJ AE.BF.CM.DQ.HU.JN.LX.PR.SZ.00000 05.16.05.12"
-- enigma: Bad plugboard : AE.BF.CM.DQ.HU.JN.LX.PR.SZ.00000
-- CallStack (from HasCallStack):
--   error, called at ./Crypto/Enigma.hs:371:57 in main:Crypto.Enigma


-- TBD: Update documentation for display functions (consolidate and expand under displayEnigmaConfig; just warning for old wrappers)
-- TBD: Test and confirm correspondence with Python
-- TBD: Better CLI error handling
-- TBD: Errors for Display functions
-- TBD: Test scripts for replacment Display functions
-- TBD: Test scripts for command line?
-- TBD: Implement operation (Display functions, using lists, and command line)
-- TBD: Version subcommand
-- TBD: Document CLI in readme
-- TBD: Help and desription text for CLI
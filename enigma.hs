import Options.Applicative      -- http://hackage.haskell.org/package/optparse-applicative
import Options.Applicative.Help.Pretty (string)     -- Necessary to format help text -- https://github.com/pcapriotti/optparse-applicative/issues/90#issuecomment-49868254
--import Data.Maybe
import Data.Monoid ((<>))       -- For GHC 8.0 through 8.2
--import Control.Exception (catch)

--import Crypto.Enigma.Utils
import Crypto.Enigma
import Crypto.Enigma.Display


cliName = "Enigma machine CLI"

data Subcommand =
        Encode { config :: String, message :: String } |
        Show { config :: String, letterO :: Maybe String, formatO :: Maybe String, highlightO :: Maybe String, encodingO :: Maybe Bool } |
        Run { config :: String, messageO :: Maybe String, formatO :: Maybe String, highlightO :: Maybe String, encodingO :: Maybe Bool, showstepsO :: Maybe Bool }
        deriving Show

data Options = Options { subcommand :: Subcommand } deriving Show

commandO = Options <$> subcommandO
--commandO = Options <$> generalOpts <*> subcommandO
-- generalOpts = strOption ( long "general" <> help "General option" )

subcommandO :: Parser Subcommand
subcommandO =
  subparser (
        command "encode" (info (Encode <$> configArg <*> messageArg <**> helper)
                         (helpText "Encode a message" "ENCODE" encodeCmdArgsFoot)) <>
        command "show"   (info (Show <$> configArg <*> letterOpt <*> formatOpt <*> highlightOpt <*> encodingOpt <**> helper)
                         (helpText "Show a machine configuration" "SHOW" showCmdArgsFoot)) <>
        command "run"    (info (Run <$> configArg <*> messageOpt <*> formatOpt <*> highlightOpt <*> encodingOpt <*> showstepOpt <**> helper)
                         (helpText "Run a machine " "Run" runCmdArgsFoot))
   )
  where
        configArg = strArgument $ metavar "CONFIG" <>
                help "Config of machine"
        messageArg = strArgument $ metavar "MESSAGE" <> help "The message"
        messageOpt = optional $ strOption ( long "message" <> short 'm' <> metavar "MESSAGE" <> value " " <>
                help messageOptHelp)
        letterOpt =  optional $ strOption ( long "letter" <> short 'l' <> metavar "LETTER" <> value " " <>
                help "Letter to highlight")
        formatOpt =  optional $ strOption ( long "format" <> short 'f' <> metavar "FORMAT" <> value "single" <>
                help "The format")
        highlightOpt =  optional $ strOption ( long "highlight" <> short 'H' <> metavar "HH" <> value "bars" <>
                help "The highlight")
        encodingOpt =  optional $ switch ( long "showencoding" <> short 'e' <>
                help "Show encoding")

        showstepOpt =  optional $ switch ( long "showstep" <> short 't' <>
                help "Show step numbers")
        stepsOpt :: Parser Int
        stepsOpt = option auto (long "steps" <> short 's' <> metavar "STEPS" <>
                help "Steps to run" )

        helpText desc cmd argsFoot = (progDesc desc <>
                header (cliName ++ ": "++ cmd ++" command") <>
                footerDoc (Just (string (unlines ["Argument notes:\n", argsFoot])))    )
                -- footer (unlines ["Shared footer. ", foot]))


main :: IO ()
main = do
--  (opts :: Options) <- execParser optsParser
    opts <- execParser optsParser
    case subcommand opts of
        Encode config message ->
                putStr $ showEnigmaEncoding (read config :: EnigmaConfig)
                        message
        Show config (Just (letter:_)) (Just format) (Just highlight) (Just showenc) ->
                putStrLn $ displayEnigmaConfig (read config :: EnigmaConfig)
                        letter format showenc (decorate highlight)
        Run config (Just message) (Just format) (Just highlight) (Just showenc) (Just showstps) ->
                putStr $ displayEnigmaOperation (read config :: EnigmaConfig)
                        message format showenc (decorate highlight) showstps
        cmd -> putStrLn $ "Unmatched command: " ++ (show cmd)
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

messageOptHelp = unlines [
         "a message to encode; characters that are not letters" ,
         "will be replaced with standard Naval substitutions or",
         "be removed"]

encodeCmdArgsFoot = unlines [configArgFoot, omitArgFoot "encode"]
showCmdArgsFoot = unlines [configArgFoot, formatArgFoot "show", highlightArgFoot, omitArgFoot "show"]
runCmdArgsFoot = unlines [configArgFoot, formatArgFoot "run", highlightArgFoot, omitArgFoot "run"]

configArgFoot = unlines [
        "CONFIG specifies an Enigma machine configuration as a string based on common",
        "historical conventions and consists of four elements, separated by spaces:",
        " + names for components, in physical order (starting with the reflector, on the",
        "   left, and ending with the 'first' rotor, on the right), separated by '-'s;",
        " + letters visible at the machine windows (in physical order);",
        " + a plugboard specification, consisting of exchanged (i.e. wired-together)",
        "   letter paris, separated by '.'s; and",
        " + the locations of ring letter A on the rotor for each rotor",
        "   (in physical order)"]

-- REV - Use of LETTER here isn't quite right for 'run' + stage vs step!
formatArgFoot cmd = unlines $ [
        "FORMAT will determine how a configuration is represented; possible values",
        "include:",
        " + 'single' (the default) which will show a single line representing the",
        "   mapping (a string in which the letter at each position indicates the letter",
        "   encoded to by letter at that position in the alphabet) preformed by the",
        "   machine as a whole, followed by window letters (as 'windows') and",
        "   positions, and indicating a letter and its encoding, if provided;",
        " + 'internal', which will show a detailed schematic of each processing stage",
        "   (proceeding from top to bottom), in which",
        "    - each line indicates the mapping (see 'single') preformed by the",
        "      component at that stage;",
        "    - each line begins with an indication of the stage (rotor number, \"P\" for",
        "      plugboard, or \"R\" for reflector), and ends with the specification of the",
        "      component at that stage;",
        "    - rotors also indicate their window letter, and position;",
        "    - the letter being encoded it is indicated as input and its encoding at",
        "      each stage is marked;",
        "   the schematic is followed by the mapping for the machine as a whole (as",
        "   'single'), and preceded by a (trivial, no-op) keyboard 'mapping'",
        "   for reference;",
        " + 'windows', which shows just the letters visible at the windows;",
        "    and",
        " + 'config', which simply shows the specification of the",
        "   configuration (in the same format as CONFIG).",
        "For formats that indicate a letter and its encoding, these will correspond to"]
        ++ letterSoruce ++
       ["The program is forgiving about forgotten format values and will accept a",
        "range of reasonable substitutes (e.g., 'detailed' or 'schematic' for",
        "'internal')."]
                where letterSoruce | cmd == "run" =
                                     ["the the letter at that step of the entry of the (symbol substituted) MESSAGE ",
                                      "provided as an argument."]
                                   | otherwise = ["the LETTER provided as an argument."]

highlightArgFoot = unlines [
        "HH can be used to determine how any encoded-to characters in mappings",
        "(see 'single' in the note on FORMAT) are highlighted. By default",
        "this highlighting is done with combining Unicode characters, which may not",
        "work on all systems, and as an alternative, any two characters provided as",
        "HH will be used to 'bracket' the highlighted character. To avoid errors,",
        "these characters should be enclosed in quotes."]


omitArgFoot cmd = unlines [
        "Note that providing no value, a value of '', or just spaces or invalid",
        "characters for " ++ omittedArg ++ " is the same as omitting it."]
                where omittedArg | cmd == "show" = "LETTER"
                                 | otherwise = "MESSAGE"


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
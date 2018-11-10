import Options.Applicative      -- http://hackage.haskell.org/package/optparse-applicative
import Options.Applicative.Help.Pretty (string)     -- Necessary to format help text -- https://github.com/pcapriotti/optparse-applicative/issues/90#issuecomment-49868254
--import Data.Maybe
import Data.Monoid ((<>))       -- For GHC 8.0 through 8.2
--import Control.Exception (catch)
--import Data.List.Split (chunksOf)
import Control.Concurrent (threadDelay)
import Control.Monad (replicateM_)
import System.Console.ANSI

--import Crypto.Enigma.Utils
import Crypto.Enigma
import Crypto.Enigma.Display


cliName_ = "Enigma Machine (Haskell crypto-enigma) CLI"

stepInterval_ = 250000

data Subcommand =
        Encode { config :: String, message :: String } |
        Show { config :: String, letterO :: Maybe String, formatO :: Maybe String, highlightO :: Maybe String, encodingO :: Maybe Bool } |
        Run { config :: String, messageO :: Maybe String, formatO :: Maybe String, highlightO :: Maybe String, encodingO :: Maybe Bool, showstepsO :: Maybe Bool, numstepsO :: Int, speedO :: Int, overwriteO :: Maybe Bool, noinitialO :: Maybe Bool }
        deriving Show

data Options = Options { subcommand :: Subcommand } deriving Show

commandO = Options <$> subcommandO
--commandO = Options <$> generalOpts <*> subcommandO
-- generalOpts = strOption ( long "general" <> help "General option" )

subcommandO :: Parser Subcommand
subcommandO =
  subparser (
        command "encode" (info (Encode <$> configArg "encode" <*> messageArg <**> helper)
                         (helpText
                         "Show the encoding of a message."
                         "ENCODE" encodeCmdArgsFoot)) <>
        command "show"   (info (Show <$> configArg "show" <*> letterOpt <*> formatOpt <*> highlightOpt <*> encodingOpt <**> helper)
                         (helpText
                         "Show an Enigma machine configuration in the specified format, optionally indicating the encoding of a specified character."
                         "SHOW" showCmdArgsFoot)) <>
        command "run"    (info (Run <$> configArg "run" <*> messageOpt <*> formatOpt <*> highlightOpt <*> encodingOpt <*> showstepOpt <*> stepsOpt <*> speedOpt <*> overwriteOpt <*> noinitialOpt <**> helper)
                         (helpText
                         "Show the operation of the Enigma machine as a series of configurations, as it encodes a message and/or for a specified number of steps. "
                         "RUN" runCmdArgsFoot))
   )
  where
        configArg cmd = strArgument $ metavar "CONFIG" <>
                help (configArgHelp cmd)
        messageArg = strArgument $ metavar "MESSAGE" <>
                help messageArgHelp
        messageOpt = optional $ strOption ( long "message" <> short 'm' <> metavar "MESSAGE" <> value " " <>
                help messageOptHelp)
        letterOpt =  optional $ strOption ( long "letter" <> short 'l' <> metavar "LETTER" <> value " " <>
                help letterOptHelp)
        formatOpt =  optional $ strOption ( long "format" <> short 'f' <> metavar "FORMAT" <> value "single" <>
                help formatOptHelp)
        highlightOpt =  optional $ strOption ( long "highlight" <> short 'H' <> metavar "HH|SPEC" <> value "bars" <>
                help highlightOptHelp)
        encodingOpt = optional $ switch ( long "showencoding" <> short 'e' <>
                help showencodingOptHelp)

        showstepOpt =  optional $ switch ( long "showstep" <> short 't' <>
                help "Show step numbers")
        stepsOpt :: Parser Int
        stepsOpt = option auto (long "steps" <> short 's' <> metavar "STEPS"  <> value (-1) <>
                help stepsOptHelp)

        speedOpt :: Parser Int
        speedOpt = option auto (long "speed" <> short 'S' <> metavar "SPEED"  <> value 0 <>
                help speedOptHelp)
        overwriteOpt = optional $ switch ( long "overwrite" <> short 'o' <>
                help overwriteOptHelp)

        noinitialOpt =  optional $ switch ( long "noinitial" <> short 'n' <>
                help noinitialOptHelp)

        helpText desc cmd argsFoot = (progDesc desc <>
                header (cliName_ ++ ": "++ cmd ++" command") <>
                footerDoc (Just (string (unlines ["Argument notes:\n", argsFoot])))    )
                -- footer (unlines ["Shared footer. ", foot]))


main :: IO ()
main = do
--  (opts :: Options) <- execParser optsParser
    opts <- execParser optsParser
    case subcommand opts of
        Encode config message ->
                putStr $ displayEnigmaEncoding (configEnigmaFromString config)
                        message
        Show config (Just (letter:_)) (Just format) (Just highlight) (Just showenc) ->
                putStrLn $ displayEnigmaConfig (configEnigmaFromString config)
                        letter
                        (displayOpts format showenc (markerFunc highlight) Nothing Nothing)
        Run config (Just message) (Just format) (Just highlight) (Just showenc) showstps stps speed (Just overwrite) (Just noinitial) ->
                mapM_ (printConfig (max speed (if overwrite then 1 else 0)) overwrite)
                                   ((if noinitial then tail else id) $ listEnigmaOperation (configEnigmaFromString config)
                                   message
                                   (displayOpts format showenc (markerFunc highlight) showstps (Just stps)))
        cmd -> putStrLn $ "Unmatched command: " ++ (show cmd)
  where
    optsParser :: ParserInfo Options
    optsParser =
        info (helper <*> commandO)
             (  fullDesc <>
                progDesc "A simple Enigma machine simulator with rich display of machine configurations." <>
                header cliName_ <>
                footer "This command line interface is part of the Haskell crypto-enigma package.")

    -- BUG: Omitted final extra line from non-overwritten internal config <<<
--     printConfig s True c = printConfig s False c >>
--                                 replicateM_ (length $ lines c) (clearLine >> cursorUpLine 1)
--     printConfig s False c = (if (length $ lines c) > 1 then putStr else putStrLn) c >>
--                                 (threadDelay (s * stepInterval_))
    printConfig s True c = printConfig s False c >>
                                replicateM_ ((length $ lines c) + (if (length $ lines c) > 1 then 1 else 0)) (clearLine >> cursorUpLine 1)
    printConfig s False c = putStrLn c >>
                                (threadDelay (s * stepInterval_))



configArgHelp cmd = case cmd of
                        "encode" -> unlines ["The machine configuration at the start of encoding (see below)"]
                        "show" -> unlines ["The machine configuration to show (see below)"]
                        "run" -> unlines ["The machine setup at the start of operation (see below)"]

messageArgHelp = messageOptHelp

messageOptHelp = unlines [
         "A message to encode; characters that are not letters" ,
         "will be replaced with standard Naval substitutions or",
         "be removed"]

letterOptHelp = unlines [
         "An optional input letter to highlight as it is" ,
         "processed by the configuration; defaults to nothing"]

formatOptHelp = unlines [
         "The format used to display machine configuration(s)" ,
         "(see below)"]

highlightOptHelp = unlines [
         "Either a pair or characters to use to highlight encoded" ,
         "characters in a machine configuration's encoding or a" ,
         "specification of a color or style (see below)"]

showencodingOptHelp = unlines [
         "Show the encoding if not normally shown for the" ,
         "specified FORMAT"]

stepsOptHelp = unlines [
        "A number of steps to run; if omitted when a message is" ,
        "provided, will default to the length of the message;" ,
        "otherwise defaults to 1"]

speedOptHelp = unlines [
        "Pause between display of each step for SPEED*" ++ show ((fromIntegral stepInterval_) / 1000000) ++ "s;",
         "1 is the minimum speed with --overwrite; defaults to 0" ,
         "otherwise"]

overwriteOptHelp = unlines [
        "Overwrite each step after a pause (may result in" ,
        "garbled output on some systems)"]

noinitialOptHelp = unlines [
        "Don't show the initial starting step"]

encodeCmdArgsFoot = init $ unlines [configArgFoot, omitArgFoot "encode"]
showCmdArgsFoot = init $ unlines [configArgFoot, formatArgFoot "show", highlightArgFoot, omitArgFoot "show"]
runCmdArgsFoot = init $ unlines [configArgFoot, formatArgFoot "run", highlightArgFoot, omitArgFoot "run"]

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
        "these characters should be enclosed in quotes. Additionally certain",
        "predefined SPEC values, such as 'red' or 'highlight' are also supported."]


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


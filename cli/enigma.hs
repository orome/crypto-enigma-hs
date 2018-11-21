{-# LANGUAGE Safe, CPP #-}
module Main where

import Options.Applicative                              -- http://hackage.haskell.org/package/optparse-applicative
import Options.Applicative.Help.Pretty  (string)        -- Necessary to format help text -- https://github.com/pcapriotti/optparse-applicative/issues/90#issuecomment-49868254
import System.Console.ANSI

import Data.Monoid                      ((<>))          -- REV: For GHC 8.0 through 8.2
import Control.Concurrent               (threadDelay)
import Control.Monad.Except             (runExcept)
import Control.Monad                    (replicateM_)

import Crypto.Enigma
import Crypto.Enigma.Display



-- Definitions  ==============================================================


-- Functions and constants ---------------------------------------------------

cliName_ = "Enigma Machine (Haskell crypto-enigma) CLI"

#if __GLASGOW_HASKELL__ < 800
cliError e = error (e ++ " (see help)")
#else
cliError e = errorWithoutStackTrace (e ++ " (see help)")
#endif

stepInterval_ = 125000


-- Command and option definitions --------------------------------------------

data Subcommand =
        Encode { config :: String, message :: String } |
        Show { config :: String, letterO ::
                Maybe String, formatO :: Maybe String, highlightO :: Maybe String, encodingO :: Maybe Bool } |
        Run { config :: String, messageO ::
                Maybe String, formatO :: Maybe String, highlightO :: Maybe String, encodingO :: Maybe Bool,
                showstepsO :: Maybe Bool, numstepsO :: Int,
                speedO :: Int, overwriteO :: Maybe Bool, noinitialO :: Maybe Bool }
        deriving Show

data Options = Options { subcommand :: Subcommand } deriving Show

commandO = Options <$> subcommandO

subcommandO :: Parser Subcommand
subcommandO =
  subparser (
        command "encode" (info (Encode <$> configArg "encode" <*> messageArg
                                <**> helper)
                         (helpText encodeCmdDesc "ENCODE" encodeCmdArgsFoot encodeCmdExamples)) <>
        command "show"   (info (Show <$> configArg "show" <*> letterOpt <*>
                                formatOpt <*> highlightOpt <*> encodingOpt
                                <**> helper)
                         (helpText showCmdDesc "SHOW" showCmdArgsFoot showCmdExamples)) <>
        command "run"    (info (Run <$> configArg "run" <*> messageOpt <*>
                                formatOpt <*> highlightOpt <*> encodingOpt <*>
                                showstepOpt <*> stepsOpt <*>
                                speedOpt <*> overwriteOpt <*> noinitialOpt
                                <**> helper)
                         (helpText runCmdDesc "RUN" runCmdArgsFoot runCmdExamples))
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

        helpText desc cmd argsFoot examplesFoot = (progDesc desc <>
                header (cliName_ ++ ": "++ cmd ++" command") <>
                footerDoc (Just $ string $ unlines ["Argument notes:\n", argsFoot, "Examples:\n", examplesFoot]))



-- Command line script =======================================================


main :: IO ()
main = do
    opts <- execParser optsParser
    case subcommand opts of
        Encode config message ->
                        putStr $ displayEnigmaEncoding (configEnigmaFromString config)
                                message
        Show config (Just (letter:_))
                (Just format) (Just highlight) (Just showenc) ->
                        putStrLn $ displayEnigmaConfig (configEnigmaFromString config)
                                letter
                                (displayOpts{format=format,showencoding=showenc,markerspec=highlight})
        Run config (Just message)
                (Just format) (Just highlight) (Just showenc)
                (Just showstps) stps
                speed (Just overwrite) (Just noinitial) ->
                        mapM_ (printConfig (max speed (if overwrite then 1 else 0)) overwrite)
                                   ((if noinitial then tail else id) $ listEnigmaOperation (configEnigmaFromString config)
                                   message
                                   (displayOpts{format=format,showencoding=showenc,markerspec=highlight,
                                                showsteps=showstps,steps=stps}))
        cmd -> putStrLn $ "Unmatched command: " ++ (show cmd)
  where
    optsParser :: ParserInfo Options
    optsParser = info (helper <*> commandO)
                      (fullDesc <> progDesc topDesc <> header cliName_ <> footerDoc (Just $ string topFoot))

    -- Like 'configEnigma' but without stack trace and with check for 4 words in a single string
    configEnigmaFromString :: String -> EnigmaConfig
    configEnigmaFromString i = if ((length $ words i) /= 4)
                          then cliError ("Enigma machine configuration has the format 'rotors windows plugboard rings'")
                          else case runExcept (configEnigmaExcept c w s r) of
                                    Right cfg  -> cfg
                                    Left err -> cliError (show err)
                                where [c, w, s, r] = words i

    printConfig s True c = printConfig s False c >>
                                replicateM_ ((length $ lines c) + (if (length $ lines c) > 1 then 1 else 0))
                                            (clearLine >> cursorUpLine 1)
    printConfig s False c = putStrLn c >>
                                (threadDelay (s * stepInterval_))



-- Help text =================================================================

topDesc = "A simple Enigma machine simulator with rich display of machine configurations."

encodeCmdDesc = "Show the encoding of a message."
showCmdDesc = "Show an Enigma machine configuration in the specified format, " ++
              "optionally indicating the encoding of a specified character."
runCmdDesc = "Show the operation of the Enigma machine as a series of configurations, " ++
             "as it encodes a message and/or for a specified number of steps. "

configArgHelp cmd = case cmd of
                        "encode" -> unlines ["The machine configuration at the start of encoding (see below)"]
                        "show" -> unlines ["The machine configuration to show (see below)"]
                        "run" -> unlines ["The machine setup at the start of operation (see below)"]
                        -- Should not happen: optparse-applicative should have caught this
                        _ -> error "ERROR: Unrecognized command: '" ++ cmd ++ "''!"

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

topFoot = "Examples:\n" ++ topExamples ++ "\nThis command line interface is part of the Haskell crypto-enigma package."

topExamples = unlines [
        "    $ enigma encode \"B-I-III-I EMO UX.MO.AY 13.04.11\" \"TESTINGXTESTINGUD\"",
        "    $ enigma encode \"B-I-III-I EMO UX.MO.AY 13.04.11\" \"TESTINGXTESTINGUD\" -f",
        "    $ enigma encode \"B-I-III-I EMO UX.MO.AY 13.04.11\" \"TESTING! testing?\" -f",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X'",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X' -H '()'",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X' -H '()' -f internal",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -s 10 -t",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -H '()'",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -H '()' -f internal",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -H '()' -f internal -o -SS",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -f config -e",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -f internal -e",
        "    $ enigma run \"c-β-VIII-VII-VI QMLI UX.MO.AY 01.13.04.11\" -s 500 -t -f internal -o",
        "",
        "More information about each of these examples is available in the help for the respective",
        "commands."]

encodeCmdExamples = init $ unlines [
        "  Encode a message:",
        "    $ enigma encode \"B-I-III-I EMO UX.MO.AY 13.04.11\" \"TESTINGXTESTINGUD\"",
        "    OZQKPFLPYZRPYTFVU",
        "    $ enigma encode \"B-I-III-I EMO UX.MO.AY 13.04.11\" \"OZQKPFLPYZRPYTFVU\"",
        "    TESTINGXTESTINGUD",
        "",
        "  Encode a message and break the output into blocks of 4:",
        "    $ enigma encode \"B-I-III-I EMO UX.MO.AY 13.04.11\" \"TESTINGXTESTINGUD\" -f",
        "    OZQK PFLP YZRP YTFV U",
        "",
        "  Standard Naval subistitutions for non-letter characters are performed",
        "  before encoding:",
        "    $ enigma encode \"B-I-III-I EMO UX.MO.AY 13.04.11\" \"TESTING! testing?\" -f",
        "    OZQK PFLP YZRP YTFV U"]

showCmdExamples = init $ unlines [
        "  Show an Enigma machine configuration as its mapping (see 'single'",
        "  in the note on FORMAT), followed by the window letters and ring settings,",
        "  and indicate how a letter is encoded; here X is encoded to T (A would be",
        "  encoded to C, B to N ... Y to W, Z to O):",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X'",
        "    X > CNAUJVQSLEMIKBZRGPHXDFYT̲̅WO  EMO  19 10 05",
        "",
        "  Use an alternate method for highlighting the encoded-to letter:",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X' -H '()'",
        "    X > CNAUJVQSLEMIKBZRGPHXDFY(T)WO  EMO  19 10 05",
        "",
        "  Show a detailed stage-by-stage schematic (see 'internal' in the note",
        "  on FORMAT) of the mappings preformed by a configuration:",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X' -H '()' -f internal",
        "    X > ABCDEFGHIJKLMNOPQRSTUVW(X)YZ",
        "      P YBCDEFGHIJKLONMPQRSTXVW(U)AZ         UX.MO.AY",
        "      1 HCZMRVJPKSUDTQOLWEXN(Y)FAGIB  O  05  I",
        "      2 KOMQEPVZNXRBDLJHFSUWYACT(G)I  M  10  III",
        "      3 AXIQJZ(K)RMSUNTOLYDHVBWEGPFC  E  19  I",
        "      R YRUHQSLDPX(N)GOKMIEBFZCWVJAT         B",
        "      3 ATZQVYWRCEGOI(L)NXDHJMKSUBPF         I",
        "      2 VLWMEQYPZOA(N)CIBFDKRXSGTJUH         III",
        "      1 WZBLRVXAYGIPD(T)OHNEJMKFQSUC         I",
        "      P YBCDEFGHIJKLONMPQRS(T)XVWUAZ         UX.MO.AY",
        "    T < CNAUJVQSLEMIKBZRGPHXDFY(T)WO",
        "",
        "  Just show the configuration in conventional format (as used in CONFIG):",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X' -f config",
        "    B-I-III-I EMO UX.MO.AY 13.04.11",
        "",
        "  As above, but show the encoding too (not shown my default for 'config'):",
        "    $ enigma show \"B-I-III-I EMO UX.MO.AY 13.04.11\" -l 'X' -f config -e",
        "    B-I-III-I EMO UX.MO.AY 13.04.11  X > T"]

runCmdExamples = init $ unlines [
        "(For details on differences among formats used for displaying each step, see the",
        "examples in the examples for the 'show' command.)",
        "",
        "  Show the operation of a machine for 10 steps, indicating step numbers (see",
        "  'single' in the note on FORMAT):",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -s 10 -t",
        "    0000      CNAUJVQSLEMIKBZRGPHXDFYTWO  EMO  19 10 05",
        "    0001      UNXKGVERLYDIQBTWMHZOAFPCJS  EMP  19 10 06",
        "    0002      QTYJZXUPKDIMLSWHAVNBGROFCE  EMQ  19 10 07",
        "    0003      DMXAPTRWKYINBLUESGQFOZHCJV  ENR  19 11 08",
        "    0004      IUSMHRPEAQTVDYWGJFCKBLOZNX  ENS  19 11 09",
        "    0005      WMVXQRLSPYOGBTKIEFHNZCADJU  ENT  19 11 10",
        "    0006      WKIQXNRSCVBOYFLUDGHZPJAEMT  ENU  19 11 11",
        "    0007      RVPTWSLKYXHGNMQCOAFDZBEJIU  ENV  19 11 12",
        "    0008      IYTKRVSMALDJHZWXUEGCQFOPBN  ENW  19 11 13",
        "    0009      PSWGMODULZVIERFAXNBYHKCQTJ  ENX  19 11 14",
        "    0010      IVOWZKHGARFSPUCMXJLYNBDQTE  ENY  19 11 15",
        "",
        "  Show the operation of a machine as it encodes a message, with step numbers:",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -H '()'",
        "    0000       CNAUJVQSLEMIKBZRGPHXDFYTWO   EMO  19 10 05",
        "    0001  T > UNXKGVERLYDIQBTWMHZ(O)AFPCJS  EMP  19 10 06",
        "    0002  E > QTYJ(Z)XUPKDIMLSWHAVNBGROFCE  EMQ  19 10 07",
        "    0003  S > DMXAPTRWKYINBLUESG(Q)FOZHCJV  ENR  19 11 08",
        "    0004  T > IUSMHRPEAQTVDYWGJFC(K)BLOZNX  ENS  19 11 09",
        "    0005  I > WMVXQRLS(P)YOGBTKIEFHNZCADJU  ENT  19 11 10",
        "    0006  N > WKIQXNRSCVBOY(F)LUDGHZPJAEMT  ENU  19 11 11",
        "    0007  G > RVPTWS(L)KYXHGNMQCOAFDZBEJIU  ENV  19 11 12",
        "",
        "  Show the operation of a machine as it encodes a message in more detail (see",
        "  'internal' in the note on FORMAT), with step numbers (only some",
        "  steps shown here):",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -H '()' -f internal",
        "    0000",
        "    ...",
        "    0001",
        "    T > ABCDEFGHIJKLMNOPQRS(T)UVWXYZ",
        "      P YBCDEFGHIJKLONMPQRS(T)XVWUAZ         UX.MO.AY",
        "      1 BYLQUIOJRTCSPNKVDWM(X)EZFHAG  P  06  I",
        "      2 KOMQEPVZNXRBDLJHFSUWYAC(T)GI  M  10  III",
        "      3 AXIQJZKRMSUNTOLYDHV(B)WEGPFC  E  19  I",
        "      R Y(R)UHQSLDPXNGOKMIEBFZCWVJAT         B",
        "      3 ATZQVYWRCEGOILNXD(H)JMKSUBPF         I",
        "      2 VLWMEQY(P)ZOANCIBFDKRXSGTJUH         III",
        "      1 YAKQUWZXFHOCSNG(M)DILJEPRTBV         I",
        "      P YBCDEFGHIJKL(O)NMPQRSTXVWUAZ         UX.MO.AY",
        "    O < UNXKGVERLYDIQBTWMHZ(O)AFPCJS",
        "    ...",
        "    0007",
        "    G > ABCDEF(G)HIJKLMNOPQRSTUVWXYZ",
        "      P YBCDEF(G)HIJKLONMPQRSTXVWUAZ         UX.MO.AY",
        "      1 IDLNWM(J)HEPXQGRYTZBUAVSFKOC  V  12  I",
        "      2 NLPDOUYMW(Q)ACKIGERTVXZBSFHJ  N  11  III",
        "      3 AXIQJZKRMSUNTOLY(D)HVBWEGPFC  E  19  I",
        "      R YRU(H)QSLDPXNGOKMIEBFZCWVJAT         B",
        "      3 ATZQVYW(R)CEGOILNXDHJMKSUBPF         I",
        "      2 KVLDPXOYNZMBHAECJ(Q)WRFSITGU         III",
        "      1 TRZBIWMHAGXCFDYJ(L)NVPSUEKOQ         I",
        "      P YBCDEFGHIJK(L)ONMPQRSTXVWUAZ         UX.MO.AY",
        "    L < RVPTWS(L)KYXHGNMQCOAFDZBEJIU",
        "",
        "  Show the steps as above, but (slowly) in place (if the platform supports it)",
        "  rather than on a new line for each; only the last step is visible on",
        "  completion (as shown here):",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -H '()' -f internal -o -SS",
        "    0007",
        "    G > ABCDEF(G)HIJKLMNOPQRSTUVWXYZ",
        "      P YBCDEF(G)HIJKLONMPQRSTXVWUAZ         UX.MO.AY",
        "      1 IDLNWM(J)HEPXQGRYTZBUAVSFKOC  V  12  I",
        "      2 NLPDOUYMW(Q)ACKIGERTVXZBSFHJ  N  11  III",
        "      3 AXIQJZKRMSUNTOLY(D)HVBWEGPFC  E  19  I",
        "      R YRU(H)QSLDPXNGOKMIEBFZCWVJAT         B",
        "      3 ATZQVYW(R)CEGOILNXDHJMKSUBPF         I",
        "      2 KVLDPXOYNZMBHAECJ(Q)WRFSITGU         III",
        "      1 TRZBIWMHAGXCFDYJ(L)NVPSUEKOQ         I",
        "      P YBCDEFGHIJK(L)ONMPQRSTXVWUAZ         UX.MO.AY",
        "    L < RVPTWS(L)KYXHGNMQCOAFDZBEJIU",
        "",
        "  Stepping a configuration only changes the window letters:",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -f config -e",
        "    0000  B-I-III-I EMO UX.MO.AY 13.04.11",
        "    0001  B-I-III-I EMP UX.MO.AY 13.04.11  T > O",
        "    0002  B-I-III-I EMQ UX.MO.AY 13.04.11  E > Z",
        "    0003  B-I-III-I ENR UX.MO.AY 13.04.11  S > Q",
        "    0004  B-I-III-I ENS UX.MO.AY 13.04.11  T > K",
        "    0005  B-I-III-I ENT UX.MO.AY 13.04.11  I > P",
        "    0006  B-I-III-I ENU UX.MO.AY 13.04.11  N > F",
        "    0007  B-I-III-I ENV UX.MO.AY 13.04.11  G > L",
        "    $ enigma run \"B-I-III-I EMO UX.MO.AY 13.04.11\" -m \"TESTING\" -t -f windows -e",
        "    0000  EMO",
        "    0001  EMP  T > O",
        "    0002  EMQ  E > Z",
        "    0003  ENR  S > Q",
        "    0004  ENS  T > K",
        "    0005  ENT  I > P",
        "    0006  ENU  N > F",
        "    0007  ENV  G > L",
        "",
        "   Watch the machine run for 500 steps:",
        "    $ enigma run \"c-β-VIII-VII-VI QMLI UX.MO.AY 01.13.04.11\" -s 500 -t -f internal -o"]

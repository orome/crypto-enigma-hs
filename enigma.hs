import Options.Applicative      -- http://hackage.haskell.org/package/optparse-applicative
import Data.Maybe

--import Crypto.Enigma.Utils
import Crypto.Enigma
import Crypto.Enigma.Display



data Subcommand =
        Encode { config :: String, message :: String } |
        Show { config :: String, letter :: Maybe String } |
        Other1 { binary :: String } |
        Other2 { regex  :: String } deriving Show

data Options = Options { subcommand :: Subcommand } deriving Show

commandO = Options <$> subcommandO
--commandO = Options <$> generalOpts <*> subcommandO

-- generalOpts = strOption ( long "general" <> help "Genera option" )

subcommandO :: Parser Subcommand
subcommandO =
  subparser (
        command "encode" (info encodeO ( progDesc "Encode a message" )) <>
        command "show" (info showO ( progDesc "Show a machine configuration" )) <>
        command "UPLOADFILE" (info other10 ( progDesc "Other dummy command 1" )) <>
        command "SEARCH" (info other20 ( progDesc "Other dummy command 2" ))
   )

encodeO = Encode <$>
  ( strArgument (metavar "CONFIG" <> help "Config of machine")) <*>
  ( strArgument (metavar "MESSAGE" <> help "The message"))

showO = Show <$>
  ( strArgument (metavar "CONFIG" <> help "Config of machine")) <*>
  ( optional $ strOption ( long "letter" <> short 'l' <> help "Letter to highlight"))

other10 = Other1 <$>
  ( strOption ( long "binary" <> help "A binary option for other command 1"))

other20 = Other2 <$>
  ( strOption ( long "regex" <> help "A regex option for other command 2"))


main :: IO ()
main = do
--  (opts :: Options) <- execParser optsParser
    opts <- execParser optsParser
    case subcommand opts of
        Encode config message -> putStr $ showEnigmaEncoding (read config :: EnigmaConfig) message
        Show config Nothing  -> putStrLn $ showEnigmaConfig (read config :: EnigmaConfig) ' '
        Show config (Just (letter:_))  -> putStrLn $ showEnigmaConfig (read config :: EnigmaConfig) letter
        Other1 _ -> putStrLn "Other command 1"
        Other2 _ -> putStrLn "Other command 2"
  where
    optsParser :: ParserInfo Options
    optsParser =
        info (helper <*> commandO)
             (  fullDesc <>
                progDesc "Command line interface to crypto-enigma package" <>
                header "Enigma machine CLI" )



-- stack exec -- enigma encode "c-β-V-VI-VIII CDTJ AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW 05.16.05.12" "FOLGENDES IST SOFORT BEKANNTZUGEBEN"
-- RBBF PMHP HGCZ XTDY GAHG UFXG EWKB LKGJ


--  main :: IO ()
-- main = execParser opt >>= print where
--   opt = info (helper <*> commandO)
--      ( fullDesc
--     <> progDesc "Example for multiple subcommands"
--     <> header "myProgram" )

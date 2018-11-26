{-# OPTIONS_HADDOCK show-extensions #-}
--{-# OPTIONS_GHC -fno-ignore-asserts #-} --REV: Use to keep asserts for valid 'Message'? <<<
{-|
Module      : Crypto.Enigma
Description : Enigma machine simulator
Copyright   : (c) 2014-2018 Roy Levien
License     : BSD3
Maintainer  : royl@aldaron.com
Stability   : experimental
Portability : POSIX

An Enigma machine simulator with rudimentary display, currently limited to the I, M3, and M4 models.

Richer display is provided by "Crypto.Enigma.Display".
-}

{-# LANGUAGE Safe #-}
module Crypto.Enigma (
        -- * Machine components
        Component,
        name,
        wiring,
        turnovers,
        Name,
        Wiring,
        Turnovers,
        component,
        rotors,
        reflectors,
         -- * Machine configurations and transitions
        EnigmaConfig,
        configEnigma',
        configEnigma,
        stages,
        components,
        positions,
        rings,
        windows,
        Position,
        Stage,
        step,
        -- * Mappings
        -- | The encodings established by the machine and its components.
        Mapping,
        Direction (..),
        componentMapping,
        stageMappingList,
        enigmaMappingList,
        enigmaMapping,
        -- * Encoding
        -- | Encoding messages.
        Message,
        message,
        enigmaEncoding
) where

import           Control.Arrow          ((&&&))
import           Control.Monad          (unless)
import           Data.Monoid            ((<>))          -- For GHC < 8.4.3 - https://stackoverflow.com/a/53024485/656912
import           Data.List              (intercalate, nub)
import           Data.List.Split        (splitOn)
import qualified Data.Map as M          (Map, fromList, union, keys, lookup, member)
import           Data.Maybe             (fromMaybe)
import           Text.Printf            (printf)
import           Data.Char              (toUpper)
import           Data.Text              (replace, pack, unpack)

import           Crypto.Enigma.Utils



-- Enigma mechanics ==========================================================


-- Machine components --------------------------------------------------------

{-|
A string identifying a 'Component' of an Enigma machine.
For rotors (including the reflector) this is one of the conventional letter or Roman numeral designations
(e.g., @\"IV\"@ or @\"β\"@). For the plugboard this is the conventional string of letter pairs (separated by periods),
indicating letters wired together by plugging (e.g., @\"AU.ZM.ZL.RQ\"@).
Absence or non-use of a plugboard can be indicated with a lone "~". See 'name'.
-}
--REV: Actually any string that does not contain periods will be taken as no plugboard; see 'component'.
type Name = String


{-|
The 'Mapping' established by the physical wiring of a 'Component', when 01 is at the window position for rotors,
and by the plug arrangement for the plugboard. See 'wiring'.
-}
type Wiring = Mapping

{-|
The list of letters on the rotor's ring that appear at the window when a 'Component''s ring is in the turnover
position.
Not applicable (and empty) for the plugboard and for reflectors. See 'turnovers'.
-}
type Turnovers = String

{-|
A component used to construct an Enigma machine (embodied in an 'EnigmaConfig') identified by its 'name', and
characterized by its physical 'wiring' and additionally — for rotors other than the reflector — by 'turnovers'
which govern the 'step'ping of the machine in which it is installed.
-}
data Component = Component {
        name :: !Name,              -- ^ The component's 'Name'.
        wiring :: !Wiring,          -- ^ The component's 'Wiring'.
        turnovers :: !Turnovers     -- ^ The component's 'Turnovers'.
}

--REV: \c -> (name c, c) instead of (name &&& id) ?
-- Definitions of rotor Components; people died for this information
rots_ :: M.Map Name Component
rots_ = M.fromList $ (name &&& id) <$> [
        -- rotors
        Component "I"    "EKMFLGDQVZNTOWYHXUSPAIBRCJ" "Q",
        Component "II"   "AJDKSIRUXBLHWTMCQGZNPYFVOE" "E",
        Component "III"  "BDFHJLCPRTXVZNYEIWGAKMUSQO" "V",
        Component "IV"   "ESOVPZJAYQUIRHXLNFTGKDCMWB" "J",
        Component "V"    "VZBRGITYUPSDNHLXAWMJQOFECK" "Z",
        Component "VI"   "JPGVOUMFYQBENHZRDKASXLICTW" "ZM",
        Component "VII"  "NZJHGRCXMYSWBOUFAIVLPEKQDT" "ZM",
        Component "VIII" "FKQHTLXOCBJSPDZRAMEWNIUYGV" "ZM",
        Component "β"    "LEYJVCNIXWPBQMDRTAKZGFUHOS" "",
        Component "γ"    "FSOKANUERHMBTIYCWLQPZXVGJD" ""]
refs_ ::  M.Map Name Component
refs_ = M.fromList $ (name &&& id) <$> [
        -- reflectors
        Component "A"    "EJMZALYXVBWFCRQUONTSPIKHGD" "",
        Component "B"    "YRUHQSLDPXNGOKMIEBFZCWVJAT" "",
        Component "C"    "FVPJIAOYEDRZXWGCTKUQSBNMHL" "",
        Component "b"    "ENKQAUYWJICOPBLMDXZVFTHRGS" "",
        Component "c"    "RDOBJNTKVEHMLFCWZAXGYIPSUQ" ""]
        -- base case (e.g. for "unplugged" plugboard, or the keyboard)
kbd_ = M.fromList [("",Component ""     "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "")]

comps_ :: M.Map Name Component
comps_ = rots_ `M.union` refs_ `M.union` kbd_

{-|
The list of valid 'Component' 'Name's for rotors.
-}
rotors :: [Name]
rotors = M.keys rots_

{-|
The list of valid 'Component' 'Name's for reflectors.
-}
reflectors :: [Name]
reflectors = M.keys refs_

{-|
The 'Component' with the specified 'Name'.
-}
component :: Name -> Component
component n = fromMaybe (Component n (foldr plug letters (splitOn "." n)) "") (M.lookup n comps_)
        -- Either lookup the rotor or reflector by name, or use the plugboard spec to
        -- generate a component with wiring in which the specified letter pairs are exchanged.
    where
        plug [p1,p2] = map (\ch -> if ch == p1 then p2 else if ch == p2 then p1 else ch)
        plug _       = id       -- Anything but a .-separated pair will have no effect (configEnigma assertion)


-- Machine configurations and transitions ------------------------------------

{-|
The (zero-based) index of the processing stage occupied by a 'Component' in an 'EngmaConfig'. See 'stages'.
-}
type Stage = Int

{-|
The generalized rotational position of a 'Component'. For rotors, this is denoted by number on the rotor
(not letter ring) that is at the "window position". For other components the only meaningful position is @1@
(see 'positions').

This (alone) determines the permutations applied to the component's 'Wiring' to produce its current 'Mapping'
(see 'componentMapping').
-}
type Position = Int

{-|
The complete description of the state of an Enigma machine, consisting of 'components', 'positions', and 'rings'.

Two functions ('configEnigma'' and 'configEnigma') are provided for creating 'EnigmaConfig', which differ in
their handling of errors in the provided arguments specifying the configuration (the only potential source of errors,
since all other arguments throught the package are coerced to valid values).
-}
data EnigmaConfig = EnigmaConfig {
        {-|
        The 'Name' of each 'Component' in an 'EnigmaConfig', in processing order.
        Unchanged by 'step'.

        >>> components $ configEnigma "c-β-V-III-II" "LQVI" "AM.EU.ZL" "16.01.21.11"
        ["AM.EU.ZL","II","III","V","\946","c"]

        (Note that any Unicode characters are
        <http://stackoverflow.com/a/24953885/656912 stored by Haskell> as their Unicode value:
        here @"\\946" == "β"@.)
        -}
        components :: ![Name],
        {-|
        The 'Position' of each 'Component' in an 'EnigmaConfig', in machine processing order.
        May be changed by 'step'.

        >>> positions $ configEnigma "c-β-V-III-II" "LQVI" "AM.EU.ZL" "16.01.21.11"
        [1,25,2,17,23,1]

        For plugboard and reflector, this will always be @1@ since the former cannot rotate,
        and the latter does not (neither will be changed by 'step'):

        prop> head (positions cfg) == 1
        prop> last (positions cfg) == 1

        This determines the encoding performed by a component (see 'componentMapping').
        -}
        positions :: ![Position],
        {-|
        The location of ring letter 'A' on the rotor for each 'Component' in an 'EnigmaConfig',
        in machine processing order.
        Unchanged by 'step'.

        >>> rings $ configEnigma "c-β-V-III-II" "LQVI" "AM.EU.ZL" "16.01.21.11"
        [1,11,21,1,16,1]

        For plugboard and reflector, this will always be @1@ since the former lacks a ring,
        and for latter ring position is irrelevant (the letter ring is not visible, and has
        no effect on when turnovers occur):

        prop> head (rings cfg) == 1
        prop> last (rings cfg) == 1
        -}
        rings :: ![Int]
        } deriving Eq

{-|
The sequential, (forward) processing-order, 'Stage' occupied by each 'Component' in an 'EnigmaConfig', starting
with @0@ for the plugboard and ending with the reflector.

>>> stages $ configEnigma "c-β-V-III-II" "LQVI" "AM.EU.ZL" "16.01.21.11"
[0,1,2,3,4,5]

prop> components cfg == ((components cfg !!) <$> stages cfg)

The term \'stage\' (lowercase) is also used here to encompass subsequent reverse processing order stages
(see, for example, 'stageMappingList').
-}
-- REV: Is this needed anywhere but in display functions? The Python version can avoid it.
stages :: EnigmaConfig -> [Stage]
stages ec = [0..(length $ components ec)-1]

-- REV: Could take a ring and a position and dispense with st
-- REV: Some properties showing only 'position' changes?
-- The letter at the window for a given stage.
-- Used in 'windows', and in 'step' to identify when a rotor ring is in the turnover position
windowLetter :: EnigmaConfig -> Stage -> Char
windowLetter ec st = chrA0 $ mod (positions ec !! st + rings ec !! st - 2) 26

{-|
Step the machine to a new 'EnigmaConfig' by rotating the rightmost (first) rotor one position, and other rotors
as determined by the 'positions' of rotors in the machine.
In the physical machine, a step occurs in response to each operator keypress,
prior to processing that key's letter. (See 'enigmaEncoding'.)

Stepping leaves the 'components', 'stages' and 'rings' of a configuration unchanged, changing only 'positions',
which is manifest in changes of the letters at the 'windows':

>>> let cfg = configEnigma "c-γ-V-I-II" "LXZO" "UX.MO.KZ.AY.EF.PL" "03.17.04.01"
>>> putStr $ unlines $ show <$> take 5 (iterate step cfg)
c-γ-V-I-II LXZO UX.MO.KZ.AY.EF.PL 03.17.04.01
c-γ-V-I-II LXZP UX.MO.KZ.AY.EF.PL 03.17.04.01
c-γ-V-I-II LXZQ UX.MO.KZ.AY.EF.PL 03.17.04.01
c-γ-V-I-II LXZR UX.MO.KZ.AY.EF.PL 03.17.04.01
c-γ-V-I-II LXZS UX.MO.KZ.AY.EF.PL 03.17.04.01

>>> let cfg = configEnigma "c-γ-V-I-II" "LXZO" "UX.MO.KZ.AY.EF.PL" "03.17.04.01"
>>> take 5 $ map windows $ iterate step cfg
["LXZO","LXZP","LXZQ","LXZR","LXZS"]

>>> let cfg = configEnigma "c-γ-V-I-II" "LXZO" "UX.MO.KZ.AY.EF.PL" "03.17.04.01"
>>> take 5 $ map positions $ iterate step cfg
[[1,15,23,8,10,1],[1,16,23,8,10,1],[1,17,23,8,10,1],[1,18,23,8,10,1],[1,19,23,8,10,1]]
-}
step :: EnigmaConfig -> EnigmaConfig
step ec = ec { positions = steppedPosition <$> stages ec } -- only positions change when stepped
    where
        -- explicit factoring to expose stepping logic
        steppedPosition :: Stage -> Position
        steppedPosition i = (mod (positions ec !! i + di - 1) 26) + 1
            where
                di | i == 0                 = 0  -- plugboard does not step
                   | i >  3                 = 0  -- only the first three rotors can step
                   | i == 1                 = 1  -- the first rotor always steps
                   | i == 2 && isTurn 2     = 1  -- the second rotor steps if it is in a turnover position
                   |           isTurn (i-1) = 1  -- others (<=3) step if previous component is in a turnover position
                   | otherwise              = 0
                isTurn :: Stage -> Bool
                isTurn j = elem (windowLetter ec j) (turnovers $ component (components ec !! j))


-- Instantiation, display, and reading ---------------------------------------

{-|
The letters at the window in an 'EnigmaConfig', in physical, conventional order.
This is the (only) visible manifestation of configuration changes during operation.

>>> windows $ configEnigma "c-β-V-III-II" "LQVI" "AM.EU.ZL" "16.01.21.11"
"LQVI"
-}
windows :: EnigmaConfig -> String
windows ec = reverse $ tail.init $ windowLetter ec <$> (stages ec)

{-|
Create an 'EnigmaConfig' from a conventional specification.

A (safe public, <https://wiki.haskell.org/Smart_constructors "smart">, total) constructor intended
for use in pure code that does validation and takes a conventional specification as input, in the form of four strings:

* The rotor 'Name's, separated by dashes (e.g. @\"C-V-I-II\"@); see 'Name'.
* The letters visible at the windows (e.g. @\"MQR\"@); see 'windows'.
* The plugboard specification (which may be omitted with  @\"~\"@); see 'Name'.
* The position of the letter ring on each rotor, separated by periods (e.g. @\"22.11.16\"@); see 'rings'.

Following convention, the elements of these strings are in physical machine order as the operator sees
them, which is the reverse of the order in which they are encountered in processing (see 'stages').

Validation is permissive, allowing for ahistorical collections and numbers of rotors (including reflectors
at the rotor stage, and trivial degenerate machines;
e.g., @configEnigma "-" \"A\" "" "01"@), and any number of (non-contradictory) plugboard wirings (including none).
Invalid arguments return an 'EnigmaError':

>>> configEnigma' "c-β-V-III-II" "LQVI" "AM.EU.ZiL" "16.01.21.11"
Left Bad plugboard: AM.EU.ZiL
-}
-- REV: Add checks for historical combinations of machine elements?
configEnigma' :: String -> String -> String -> String -> Either EnigmaError EnigmaConfig
configEnigma' rots winds plug rngs = do
        unless (and $ (==(length components')) <$> [length winds', length rngs']) (Left BadNumbers)
        unless (rngs == (filter (`elem` "0123456789.") rngs)) (Left (BadRings rngs))
        unless (and $ [(>=1),(<=26)] <*> rngs') (Left (BadRings rngs))
        unless (and $ (`elem` letters) <$> winds') (Left (BadWindows winds))
        unless (plug `elem` ["~",""," "] ||
                       ((and $ (==2).length <$> splitOn "." plug) &&
                        (and $ (`elem` letters) <$> filter (/='.') plug) &&
                        ((\s -> s == nub s) $ filter (/='.') plug))
               ) (Left (BadPlugs plug))
        unless (and $ (`M.member` comps_) <$> tail components')
                (Left (BadComponents rots $ unwords $ filter (`notElem` (rotors ++ reflectors)) (init (tail components'))                                                 ))
        -- REV: Disallow no-op "keyboard" as component; disallow rotors as reflectors and vice versa <<<
--         unless (and $ (`M.member` (rots_ `M.union` refs_)) <$> tail components')
--                 (Left (BadComponents rots $ unwords $ filter (`notElem` (rotors ++ reflectors)) (init (tail components'))                                                 ))
--         unless (and $ (`M.member` rots_) <$> init (tail components'))
--                 (Left (BadRotors rots $ unwords $ filter (`notElem` rotors) (init (tail components'))))
--         unless ((last $ components') `M.member` refs_)
--                 (Left (BadReflector rots (last $ components')))
        Right EnigmaConfig {
                components = components',
                positions = zipWith (\w r -> (mod (numA0 w - r + 1) 26) + 1) winds' rngs',
                rings = rngs'
        }
    where
        -- Order is reversed, from physical to processing order
        -- Rings and windows are padded with values for the plugboard and reflector
        rngs' = reverse $ (read <$> (splitOn "." $ "01." ++ rngs ++ ".01") :: [Int])
        winds' = "A" ++ reverse winds ++ "A"
        components' = reverse $ splitOn "-" $ rots ++ "-" ++ plug

-- Errors for use in configEnigma'
data EnigmaError = BadNumbers
                 | BadRings String
                 | BadWindows String
                 | BadPlugs String
                 | BadComponents String String
                 | BadRotors String String
                 | BadReflector String String
                 | MiscError String

instance Show EnigmaError where
        show BadNumbers = "Numbers of windows, ring settings, and components don't match"
        show (BadRings s) = "Bad ring settings: " ++ s
        show (BadWindows s) = "Bad windows: " ++ s
        show (BadPlugs s) = "Bad plugboard: " ++ s
        show (BadComponents arg s) = "Bad components: " ++ s ++ " in " ++ arg
        show (BadRotors arg s) = "Bad rotors: " ++ s ++ " in " ++ arg
        show (BadReflector arg s) = "Bad reflector: " ++ s ++ " in " ++ arg
        show (MiscError s) = s

{-|
Create an 'EnigmaConfig' from a conventional specification.

A thin convenience wrapper on @configEnigma'@ intended for most uses (e.g., interactive) that takes the same
arguments but errors with an informative message and a stack trace:

>>> configEnigma "c-β-V-III-II" "LQVI" "AM.EU.ZiL" "16.01.21.11"
*** Exception: Bad plugboard : AM.EU.ZiL
CallStack (from HasCallStack):
  error, called at crypto-enigma/Crypto/Enigma.hs:317:21 in main:Crypto.Enigma

This should be used instead of @read@, which cannot report error details:

>>> read "c-β-V-III-II LQVI AM.EU.ZiL 16.01.21.11" :: EnigmaConfig
*** Exception: Prelude.read: no parse
-}
configEnigma :: String -> String -> String -> String -> EnigmaConfig
configEnigma rots winds plug rngs = either (error.show) id (configEnigma' rots winds plug rngs)

{-|
Read the elements of a conventional specification (see 'configEnigma') as a single string.

>>> let cfgstr = "c-β-V-III-II LQVI AM.EU.ZL 16.01.21.11"
>>> read cfgstr == (\[c, w, s, r] -> configEnigma c w s r) (words cfgstr)
True
-}
instance Read EnigmaConfig where
        readsPrec _ i = if ((length $ words i) /= 4)
                          then []
                          else case configEnigma' c w s r of
                                            Right cfg  -> [(cfg, "")]
                                            Left _ -> []
                                   where [c, w, s, r] = words i

{-|
Show the elements of a conventional specification (see 'configEnigma'') joined by spaces into a single string.

>>> configEnigma "b-β-V-VIII-II" "XQVI" "UX.MO.KZ.AY.EF.PL" "03.17.24.11"
"b-β-V-VIII-III XQVI UX.MO.KZ.AY.EF.PL 03.17.24.11"
-}
instance Show EnigmaConfig where
        show ec = unwords [intercalate "-" $ reverse.tail $ components ec,
                           windows ec,
                           head $ components ec,
                           intercalate "." $ reverse.tail.init $ (printf "%02d") <$> (rings ec)]

{-|
Show a 'Component' as a formatted string consisting of its 'name', 'wiring', and 'turnovers' (if any).
-}
instance Show Component where
        show c = printf "%-5.5s" (name c) ++ " " ++ wiring c ++ " " ++ turnovers c



-- Mapping ==================================================================

{-|
The mapping used by a component (see 'wiring' and 'componentMapping')
or by the machine (see 'enigmaMapping') to perform a
<https://en.wikipedia.org/wiki/Substitution_cipher#Simple_substitution simple substitution encoding>.

This is expressed as a string of letters indicating the mapped-to letter
for the letter at that position in the alphabet — i.e., as a permutation of the alphabet.
For example, the mapping @EKMFLGDQVZNTOWYHXUSPAIBRCJ@ encodes @A@ to @E@, @B@ to @K@, @C@ to @M@, ...
@Y@ to @C@, and @Z@ to @J@.
-}
--REV: Enforce as a class (in encoding functions too)' (#12) <<
type Mapping = String


-- Enigma encoding logic -----------------------------------------------------

{-|
The direction that a signal flows through a 'Component'. During encoding of a character, the signal
passes first through the wiring of each component, from right to left in the machine, in a forward ('Fwd')
direction, then through the reflector, and then, from left to right, through each component again,
in reverse ('Rev').

This direction affects the encoding performed by the component (see 'componentMapping').
-}
--REV: These only need to be exposed to allow componentMapping to be exposed; necessary? <<<
data Direction = Fwd | Rev

{-|
The 'Mapping' performed by a 'Component' as a function of its 'Position' and the 'Direction' of the
signal passing through it.

The base encoding of a 'Component', performed with rotor position @1@ at the window, is set by its 'wiring'.

prop>  componentMapping Fwd comp 1 == wiring comp

For all other positions, the encoding is a cyclic permutation this mapping's inputs (backward)
and outputs (forward) by the rotational offset of the rotor away from the @1@ position
(though in an actual 'EmigmaConfig' such positions occur only for rotors; see 'positions').

Note that because the wiring of reflectors generates mappings that consist entirely of paired exchanges
of letters, reflectors (at any position) produce the same mapping in both directions (the same is true
of the plugboard):

>>> let tst c n = componentMapping Fwd (component c) n == componentMapping Rev (component c) n
>>> and $ tst <$> ["A","B","C","b","c"] <*> [1..26]
True
-}
--REV: Add assertion to make sure plugboard is not rotated ; assert not in keys?
componentMapping:: Direction -> Component -> Position -> Mapping
componentMapping d c p = case d of
        Fwd -> map (\ch -> rotMap (1-p) letters !! (numA0 ch)) (rotMap (p-1) (wiring c))
        Rev -> chrA0 <$> (ordering $ componentMapping Fwd c p)
    where
        rotMap :: Int -> Wiring -> Mapping
        rotMap o w = take 26 . drop (mod o 26) . cycle $ w

{-|
The list of 'Mapping's for each stage of an 'EnigmaConfig': the encoding performed by the
'Component' /at that point/ in the progress through the machine.

These are arranged in processing order, beginning with the encoding performed by the plugboard,
followed by the forward encoding performed by each rotor (see 'componentMapping'), then the reflector,
followed by the reverse encodings by each rotor, and finally by the plugboard again.

>>> putStr $ unlines $ stageMappingList (configEnigma "b-γ-V-VIII-II" "LFAQ" "UX.MO.KZ.AY.EF.PL" "03.17.04.11")
YBCDFEGHIJZPONMLQRSTXVWUAK
LORVFBQNGWKATHJSZPIYUDXEMC
BJYINTKWOARFEMVSGCUDPHZQLX
ILHXUBZQPNVGKMCRTEJFADOYSW
YDSKZPTNCHGQOMXAUWJFBRELVI
ENKQAUYWJICOPBLMDXZVFTHRGS
PUIBWTKJZSDXNHMFLVCGQYROAE
UFOVRTLCASMBNJWIHPYQEKZDXG
JARTMLQVDBGYNEIUXKPFSOHZCW
LFZVXEINSOKAYHBRGCPMUDJWTQ
YBCDFEGHIJZPONMLQRSTXVWUAK

Note that, because plugboard 'Mapping' is established by paired exchanges of letters
(see 'componentMapping'),

prop> head (stageMappingList cfg) == last (stageMappingList cfg)

As noted (see 'stages') the term \'stage\' here encompasses reverse processing:

prop> length (stageMappingList cfg) == 2 * length (stages cfg) - 1

A richer example of how this list is used, and how it can be interpreted,
can be found in "Crypto.Enigma.Display#displayEnigmaConfigInternalEG".
-}
stageMappingList:: EnigmaConfig -> [Mapping]
stageMappingList ec = ((stageMapping Fwd) <$>) <> ((stageMapping Rev) <$>).tail.reverse $ stages ec
    where
        stageMapping :: Direction -> Stage -> Mapping
        stageMapping d sn = componentMapping d (component $ components ec !! sn) (positions ec !! sn)

{-|
The list of 'Mapping's an 'EnigmaConfig' has performed by each stage:
the encoding performed by the 'EnigmaConfig' /up to that point/ in the progress through the machine.

>>> putStr $ unlines $ enigmaMappingList (configEnigma "b-γ-V-VIII-II" "LFAQ" "UX.MO.KZ.AY.EF.PL" "03.17.04.11")
YBCDFEGHIJZPONMLQRSTXVWUAK
MORVBFQNGWCSJHTAZPIYEDXULK
EVCHJTGMKZYUAWDBXSOLNIQPFR
UDHQNFZKVWSAIOXLYJCGMPTRBE
BKNUMPIGREJYCXLQVHSTOAFWDZ
NCBFPMJYXAIGKRODTWZVLEUHQS
HIUTFNSAOPZKDVMBGREYXWQJLC
CAEQTJYUWIGMVKNFLPRXDZHSBO
RJMXFBCSHDQNOGELYUKZTWVPAI
COYWEFZPNVGHBIXATUKQMJDRLS
CMAWFEKLNVGHBIUYTXZQOJDRPS

Since these may be thought of as cumulative encodings,

prop> enigmaMapping cfg == last (enigmaMappingList cfg)
-}
enigmaMappingList :: EnigmaConfig -> [Mapping]
enigmaMappingList ec = scanl1 (flip encode') (stageMappingList ec)

{-|
The 'Mapping' performed by the Enigma machine.

>>> enigmaMapping (configEnigma "b-γ-V-VIII-II" "LFAQ" "UX.MO.KZ.AY.EF.PL" "03.17.04.11")
"CMAWFEKLNVGHBIUYTXZQOJDRPS"

A example of a richer display of this information can be found in "Crypto.Enigma.Display#displayEnigmaSingleEG".
-}
enigmaMapping :: EnigmaConfig -> Mapping
enigmaMapping ec = last $ enigmaMappingList ec  -- The final stage's progressive encoding

{-|
Encode a 'Message' using a given (starting) machine configuration, by 'step'ping the configuration prior to
processing each character of the message. This produces a new configuration (with new 'positions' only)
for encoding each character, which serves as the "starting" configuration for subsequent
processing of the message.

>>> enigmaEncoding (configEnigma "b-γ-V-VIII-II" "LFAP" "UX.MO.KZ.AY.EF.PL" "03.17.04.11") "KRIEG"
"GOWNW"

The details of this encoding and its relationship to stepping from one configuration to another are
illustrated in "Crypto.Enigma.Display#displayEnigmaOperationEG".

Note that because of the way the Enigma machine is designed, it is always the case (provided that 'msg' is
all uppercase letters) that

prop> enigmaEncoding cfg (enigmaEncoding cfg msg) == msg
-}
enigmaEncoding :: EnigmaConfig -> Message -> String
enigmaEncoding ec str =
        -- The encoding of a string is the sequence encodings of each character
        -- performed by sequentially stepped configurations (preceded by a step)
        zipWith encode (enigmaMapping <$> cfgs) (message str) where cfgs = iterate step (step ec)


-- Message entry -------------------------------------------------------------

{-|
A (<https://wiki.haskell.org/Type_synonym synonym> for) 'String', indicating that 'message' will be applied
to the corresponding argument.
-}
type Message = String

{-|
Convert a 'String' to valid Enigma machine input: replace any symbols for which there are standard Kriegsmarine
substitutions, remove any remaining non-letter characters, and convert to uppercase. This function is applied
automatically to 'String's suppied as 'Message' arguments to functions in this package.
-}
-- REV: Awkward pack/unpack patch to remove dependency on MissingH (#29)
message :: String -> Message
message s = filter (`elem` letters) $ foldl1 fmap (uncurry replace' <$> subs) $ toUpper <$> s
    where
        subs = [(" ",""),(".","X"),(",","Y"),("'","J"),(">","J"),("<","J"),("!","X"),
                ("?","UD"),("-","YY"),(":","XX"),("(","KK"),(")","KK"),
                ("1","YQ"),("2","YW"),("3","YE"),("4","YR"),("5","YT"),
                ("6","YZ"),("7","YU"),("8","YI"),("9","YO"),("0","YP")]
        replace' a b c = unpack $ replace (pack a) (pack b) (pack c)

-- REV: Rejected (#12) alternate version in which 'Message' is at class (and caller is responsible for making Message).
-- data Message = Message String deriving Show
--
-- message :: String -> Message
-- message s = Message (filter (`elem` letters) $ foldl1 fmap (uncurry replace <$> subs) $ toUpper <$> s)
--     where
--         subs = [(" ",""),(".","X"),(",","Y"),("'","J"),(">","J"),("<","J"),("!","X"),
--                 ("?","UD"),("-","YY"),(":","XX"),("(","KK"),(")","KK"),
--                 ("1","YQ"),("2","YW"),("3","YE"),("4","YR"),("5","YT"),
--                 ("6","YZ"),("7","YU"),("8","YI"),("9","YO"),("0","YP")]

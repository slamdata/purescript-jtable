module Data.Json.JSemantic
  ( JSemantic(..)
  , JSemanticParsers(..)
  , TimeRec(..)
  , toSemantic
  , toSemanticDef
  , defaultParsers
  , renderJSemantic
  ) where

import Prelude

import Control.Plus (empty)
import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)

import Data.Argonaut.JCursor (JsonPrim(), runJsonPrim)
import Data.Array as A
import Data.Foldable (foldMap)
import Data.Either (fromRight)
import Data.Int (fromNumber)
import Data.List ((:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe.First (First(..))
import Data.Newtype (alaF)

import Data.String (Pattern(..), split, stripSuffix, length)
import Data.String.Regex (Regex(), regex, match, parseFlags, replace)
import Data.Time.Duration as Time
import Data.JSDate as JSDate

import Partial.Unsafe (unsafePartial)

type TimeRec =
  { hours ∷ Time.Hours
  , minutes ∷ Time.Minutes
  , seconds ∷ Time.Seconds
  , milliseconds ∷ Time.Milliseconds
  }

timeRecToString ∷ TimeRec → String
timeRecToString
  { hours: (Time.Hours h)
  , minutes: (Time.Minutes m)
  , seconds: (Time.Seconds s)
  , milliseconds: (Time.Milliseconds ms)
  } =
    print h <> ":" <> print m <> ":" <> print s <>
      if ms > 0.0 then ("." <> print ms) else ""
  where
  print n =
    let
      s' = show n
      s'' = fromMaybe s' (stripSuffix (Pattern ".0") s')
    in if length s'' < 2 then "0" <> s'' else s''

data JSemantic
  = Integral Int
  | Fractional Number
  | Date JSDate.JSDate
  | DateTime JSDate.JSDate
  | Time
      { hours ∷ Time.Hours
      , minutes ∷ Time.Minutes
      , seconds ∷ Time.Seconds
      , milliseconds ∷ Time.Milliseconds
      }
  | Interval JSDate.JSDate JSDate.JSDate
  | Text String
  | Bool Boolean
  | Percent Number
  | Currency String Number
  | NA

instance showJSemantic ∷ Show JSemantic where
  show (Integral n) = "(Integral " <> show n <> ")"
  show (Fractional n) = "(Fractional " <> show n <> ")"
  show (Date d) = "(Date " <> JSDate.toString d <> ")"
  show (DateTime d) = "(DateTime " <> JSDate.toString d <> ")"
  show (Time d) = "(Time " <> timeRecToString d <> ")"
  show (Interval u v) = "(Interval " <> JSDate.toString u <> " " <> JSDate.toString  v <> ")"
  show (Text s) = "(Text " <> show s <> ")"
  show (Bool b) = "(Bool " <> show b <> ")"
  show (Percent n) = "(Percent " <> show n <> ")"
  show (Currency c n) = "(Currency " <> show n <> show c <> ")"
  show NA = "NA"

instance eqJSemantic ∷ Eq JSemantic where
  eq (Integral a) (Integral b) = a == b
  eq (Fractional a) (Fractional b) = a == b
  eq (Date a) (Date b) = JSDate.toString a == JSDate.toString b
  eq (DateTime a) (DateTime b) = JSDate.toString a == JSDate.toString b
  eq (Time a) (Time b) =
    a.hours == b.hours &&
    a.minutes == b.minutes &&
    a.seconds == b.seconds &&
    a.milliseconds == b.milliseconds
  eq (Interval u v) (Interval x y)
    = (JSDate.toString u == JSDate.toString x)
    && (JSDate.toString v == JSDate.toString y)
  eq (Text a) (Text b) = a == b
  eq (Bool a) (Bool b) = a == b
  eq (Percent a) (Percent b) = a == b
  eq (Currency c a) (Currency c' b) = a == b && c == c'
  eq (NA) (NA) = true
  eq _ _ = false

renderJSemantic ∷ JSemantic → String
renderJSemantic = case _ of
  (Integral n) → show n
  (Fractional n) → show n
  (Date d) → JSDate.toString d
  (DateTime d) → JSDate.toString d
  (Time d) → timeRecToString d
  (Interval u v) → JSDate.toString u <> " - " <> JSDate.toString v
  (Text s) → s
  (Bool b) → show b
  (Percent n) → show n <> "%"
  (Currency c n) → show c <> show n
  NA → ""

type JSemanticParsers =
  { boolParsers ∷ L.List (Boolean → Maybe JSemantic)
  , numberParsers ∷ L.List (Number → Maybe JSemantic)
  , stringParsers ∷ L.List (String → Maybe JSemantic)
  }

rx ∷ String → Regex
rx s = unsafePartial $ fromRight $ regex s $ parseFlags ""

rg ∷ String → Regex
rg s = unsafePartial $ fromRight $ regex s $ parseFlags "g"

foreign import s2nImpl ∷ (Number → Maybe Number) → Maybe Number → String → Maybe Number

s2n ∷ String → Maybe Number
s2n = s2nImpl Just Nothing

-- source: http://www.fileformat.info/info/unicode/category/Sc/list.htm
currencySymbols ∷ String
currencySymbols = """([\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6])"""

-- the first capture group will be used
currencyNums ∷ String
currencyNums = """(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)"""

rxDateTime ∷ Regex
rxDateTime = rx """^((\d{4})-(\d{2})-(\d{2})T(\d{2})\:(\d{2})\:(\d{2})Z?[+-](\d{2})\:(\d{2}))$"""

rxTime ∷ Regex
rxTime = rx """^([01]?[0-9]|2[0-3]):([0-5][0-9])(:([0-5][0-9]))*(.([0-9]{3}))*$"""

rxPercent ∷ Regex
rxPercent = rx """^(-?\d+(\.\d+)?)\%$"""

rxGuardNum ∷ Regex
rxGuardNum = rx """^[+-]?([1-9]|0?\.|0$)"""

integralNum ∷ Number → Maybe JSemantic
integralNum n = Integral <$> fromNumber n

fractionalNum ∷ Number → Maybe JSemantic
fractionalNum n = pure $ Fractional n

percentString ∷ String → Maybe JSemantic
percentString str = do
  matched ← match rxPercent str
  s1 ← matched A.!! 1
  num ← s1 >>= s2n
  pure $ Percent num

currencyLeftString ∷ String → Maybe JSemantic
currencyLeftString str = do
  matched ← match (rx $ "^" <> currencySymbols <> currencyNums <> "$") str
  s1 ← matched A.!! 2
  cur ← matched A.!! 1 >>= id
  num ← (replace (rg ",") "" <$> s1) >>= s2n
  pure $ Currency cur num

currencyRightString ∷ String → Maybe JSemantic
currencyRightString str = do
  matched ← match (rx $ "^" <> currencyNums <> currencySymbols <> "$") str
  s1 ← matched A.!! 1
  cur ← matched A.!! 5 >>= id
  num ← (replace (rg ",") "" <$> s1) >>= s2n
  pure $ Currency cur num

timeString ∷ String → Maybe JSemantic
timeString str = do
  matched ← match rxTime str
  h ← matched A.!! 1 >>= id >>= s2n
  m ← matched A.!! 2 >>= id >>= s2n
  let s = maybe zero Time.Seconds $ matched A.!! 4 >>= id >>= s2n
      ms = maybe zero Time.Milliseconds $ matched A.!! 6 >>= id >>= s2n
  pure $ Time
    { hours: Time.Hours h
    , minutes: Time.Minutes m
    , seconds: s
    , milliseconds: ms
    }

datetimeString ∷ String → Maybe JSDate.JSDate
datetimeString str = do
  matched ← match rxDateTime str
  s1 ← matched A.!! 1 >>= id
  let d = runPure $ unLocale $ JSDate.parse s1
  if JSDate.isValid d then Just d else Nothing
  where
  -- It's safe to do this here, because the parser is guarded with a regex that
  -- ensures the format will parse as UTC or with a specific locale.
  -- Still... yuck.
  unLocale ∷ ∀ a. Eff (locale ∷ JSDate.LOCALE) a → Eff () a
  unLocale = unsafeCoerceEff

intervalString ∷ String → Maybe JSemantic
intervalString s = do
  let
    arr ∷ Array String
    arr = split (Pattern " - ") s
  s1 ← arr A.!! 0
  s2 ← arr A.!! 1
  d1 ← datetimeString s1
  d2 ← datetimeString s2
  pure $ Interval d1 d2

integralString ∷ String → Maybe JSemantic
integralString s = do
  _ ← match rxGuardNum s
  Integral <$> (s2n s >>= fromNumber)

fractionalString ∷ String → Maybe JSemantic
fractionalString s = do
  _ ← match rxGuardNum s
  Fractional <$> s2n s

textString ∷ String → Maybe JSemantic
textString s = pure $ Text s

stringParsers ∷ L.List (String → Maybe JSemantic)
stringParsers
  = integralString
  : fractionalString
  : percentString
  : currencyLeftString
  : currencyRightString
  : timeString
  : (map DateTime <<< datetimeString)
  : intervalString
  : textString
  : empty

defaultParsers ∷ JSemanticParsers
defaultParsers =
  { boolParsers: L.singleton (pure <<< Bool)
  , numberParsers: integralNum : fractionalNum : empty
  , stringParsers: stringParsers
  }

toSemantic ∷ JSemanticParsers → JsonPrim → JSemantic
toSemantic o p =
  runJsonPrim p
    (const NA)
    (applyParsers o.boolParsers)
    (applyParsers o.numberParsers)
    (applyParsers o.stringParsers)
  where
  applyParsers ∷ ∀ a. L.List (a → Maybe JSemantic) → a → JSemantic
  applyParsers lst a =
    fromMaybe NA $ alaF First foldMap (_ $ a) lst

toSemanticDef ∷ JsonPrim → JSemantic
toSemanticDef = toSemantic defaultParsers

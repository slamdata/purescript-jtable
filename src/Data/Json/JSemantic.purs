module Data.Json.JSemantic  
  ( JSemantic(..), JSemanticRegexes(), toSemantic, toSemanticDef
  , JSemanticOpts(..), jSemanticOptsDefault, renderJSemantic
  ) where

import Prelude
import Data.Argonaut.JCursor(JsonPrim(..), runJsonPrim)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Array ((!!))
import Data.String (split)
import Data.String.Regex (Regex(..), regex, test, match, noFlags, parseFlags, replace)
import Data.Int (fromNumber)
import Control.MonadPlus (guard)
import qualified Data.Date as Date
import Control.Alt ((<|>))
import Math (floor)


data JSemantic = Integral   Int
               | Fractional Number
               | Date       Date.Date
               | DateTime   Date.Date
               | Time       Date.Date
               | Interval   Date.Date Date.Date
               | Text       String
               | Bool       Boolean
               | Percent    Number
               | Currency   Number
               | NA

instance showJSemantic :: Show JSemantic where
  show (Integral   n) = "(Integral " ++ show n ++ ")"
  show (Fractional n) = "(Fractional " ++ show n ++ ")"
  show (Date       d) = "(Date " ++ show d ++ ")"
  show (DateTime   d) = "(DateTime " ++ show d ++ ")"
  show (Time       d) = "(Time " ++ show d ++ ")"
  show (Interval u v) = "(Interval " ++ show u ++ " " ++ show v ++ ")"
  show (Text       s) = "(Text " ++ show s ++ ")"
  show (Bool       b) = "(Bool " ++ show b ++ ")"
  show (Percent    n) = "(Percent " ++ show n ++ ")"
  show (Currency   n) = "(Currency " ++ show n ++ ")"
  show (NA)           = "NA"

instance eqJSemantic :: Eq JSemantic where
  eq (Integral   a) (Integral   b) = a == b
  eq (Fractional a) (Fractional b) = a == b
  eq (Date       a) (Date       b) = a == b
  eq (DateTime   a) (DateTime   b) = a == b
  eq (Time       a) (Time       b) = a == b
  eq (Interval u v) (Interval x y) = (u == x) && (v == y)
  eq (Text       a) (Text       b) = a == b
  eq (Bool       a) (Bool       b) = a == b
  eq (Percent    a) (Percent    b) = a == b
  eq (Currency   a) (Currency   b) = a == b
  eq (NA) (NA) = true
  eq _ _ = false

renderJSemantic :: JSemantic -> String
renderJSemantic j = case j of
  (Integral   n) -> show n
  (Fractional n) -> show n
  (Date       d) -> show d
  (DateTime   d) -> show d
  (Time       d) -> show d
  (Interval u v) -> show u ++ " - " ++ show v
  (Text       s) -> s
  (Bool       b) -> show b
  (Percent    n) -> show n ++ "%"
  (Currency   n) -> "$" ++ show n
  (NA)           -> ""

type JSemanticRegexes =
  { percent :: Regex
  , currency :: Regex
  , date :: Regex
  }

type JSemanticOpts = 
  { regexps :: JSemanticRegexes
  }

-- source: http://www.fileformat.info/info/unicode/category/Sc/list.htm
currencySymbols :: String
currencySymbols = """[\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]"""

-- the first capture group will be used 
currencyNums :: String
currencyNums = """?(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)$"""

dateStr :: String
dateStr = """^((\d{4})-(\d{2})-(\d{2})T(\d{2})\:(\d{2})\:(\d{2})Z?[+-](\d{2})\:(\d{2}))$"""

percentStr :: String
percentStr = """^(-?\d+(\.\d+)?)\%$"""


jSemanticOptsDefault :: JSemanticOpts
jSemanticOptsDefault = 
  { regexps: { percent: rx percentStr
             , currency: rx $ "^" ++ currencySymbols ++ currencyNums
             , date: rx dateStr
             }
  } 

rx s = regex s noFlags
rg s = regex s $ parseFlags "g"

analyzeNum :: Number -> JSemantic
analyzeNum n = maybe (Fractional n) Integral $ fromNumber n


foreign import s2nImpl :: (Number -> Maybe Number) -> Maybe Number -> String -> Maybe Number

s2n :: String -> Maybe Number
s2n = s2nImpl Just Nothing

parseX :: forall a. Regex -> (String -> Maybe a) -> (a -> JSemantic) -> String ->
          Maybe JSemantic
parseX regexp parser constr s = do
  matched <- match regexp s
  s1 <- matched !! 1
  constr <$> (s1 >>= parser)

parsePercent :: Regex -> String -> Maybe JSemantic 
parsePercent  r = parseX r s2n Percent

parseCurrency :: Regex -> String -> Maybe JSemantic
parseCurrency r = parseX r (replace (rg ",") "" >>> s2n) Currency

parseDateTime :: Regex -> String -> Maybe JSemantic
parseDateTime r = parseX r Date.fromString DateTime 

parseInterval :: Regex -> String -> Maybe JSemantic
parseInterval r s = do
  let arr = split " - " s
  s1 <- arr !! 0
  s2 <- arr !! 1
  guard $ (test r s1) && (test r s2)
  d1 <- Date.fromString s1
  d2 <- Date.fromString s2
  pure $ Interval d1 d2

-- analyzeStr :: {regexps} -> String -> JSemantic
analyzeStr :: JSemanticRegexes -> String -> JSemantic
analyzeStr rs s = fromMaybe (Text s) (    parsePercent rs.percent s
                                      <|> parseCurrency rs.currency s
                                      <|> parseDateTime rs.date s
                                      <|> parseInterval rs.date s
                                     )
  -- TODO: date, time

toSemantic :: JSemanticOpts -> JsonPrim -> JSemantic
toSemantic o p = runJsonPrim p (const NA) Bool analyzeNum (analyzeStr o.regexps)

toSemanticDef :: JsonPrim -> JSemantic
toSemanticDef = toSemantic jSemanticOptsDefault


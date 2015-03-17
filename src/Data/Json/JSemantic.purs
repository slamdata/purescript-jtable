module Data.Json.JSemantic
  ( JSemantic(..), toSemantic, toSemanticDef
  , JSemanticOpts(..), jSemanticOptsDefault
  ) where

import Data.Argonaut.JCursor(JsonPrim(..), runJsonPrim)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (head, tail)
import Data.String (split)
import Data.String.Regex (Regex(..), regex, test, match, noFlags, parseFlags, replace)
import qualified Data.Date as Date
import Control.Alt ((<|>))
import Math (floor)


data JSemantic = Integral   Number
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
  (==) (Integral   a) (Integral   b) = a == b
  (==) (Fractional a) (Fractional b) = a == b
  (==) (Date       a) (Date       b) = a == b
  (==) (DateTime   a) (DateTime   b) = a == b
  (==) (Time       a) (Time       b) = a == b
  (==) (Interval u v) (Interval x y) = (u == x) && (v == y)
  (==) (Text       a) (Text       b) = a == b
  (==) (Bool       a) (Bool       b) = a == b
  (==) (Percent    a) (Percent    b) = a == b
  (==) (Currency   a) (Currency   b) = a == b
  (==) (NA) (NA) = true
  (==) _ _ = false
  (/=) a b = not (a == b)

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

type JSemanticOpts = {
  regexps :: {
    percent :: Regex,
    currency :: Regex,
    date :: Regex } }

-- source: http://www.fileformat.info/info/unicode/category/Sc/list.htm
currency_symbols = """[\$\u20A0-\u20CF\u00A2\u00A3\u00A4\u00A5\u058F\u060B\u09F2\u09F3\u09FB\u0AF1\u0BF9\u0E3F\u17DB\uA838\uFDFC\uFE69\uFF04\uFFE0\uFFE1\uFFE5\uFFE6]"""

jSemanticOptsDefault = {
  regexps : { -- the first capture group will be used
    percent: rx """^(-?\d+(\.\d+)?)\%$""",
    currency: rx $ "^" ++ currency_symbols ++ """?(([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?)$""",
    date: rx """^((\d{4})-(\d{2})-(\d{2})T(\d{2})\:(\d{2})\:(\d{2})Z?[+-](\d{2})\:(\d{2}))$"""
}} :: JSemanticOpts

rx s = regex s noFlags
rg s = regex s $ parseFlags "g"


analyzeNum :: Number -> JSemantic
analyzeNum n | floor n == n = Integral n
analyzeNum n                = Fractional n

foreign import _s2n """var _s2n = function (Just) { return function(Nothing) {
  return function (s) { 
    var n = s * 1; if (isNaN(n)) {return Nothing} else {return Just(n)} 
}}}""" :: (Number -> Maybe Number) -> Maybe Number -> String -> Maybe Number

s2n :: String -> Maybe Number
s2n = _s2n Just Nothing

parseX :: forall a. Regex -> (String -> Maybe a) -> (a -> JSemantic) -> String -> Maybe JSemantic
parseX regexp parser constr s = 
  case match regexp s of
    Just (_ : s1 : _) -> constr <$> parser s1
    _                 -> Nothing

parsePercent  r = parseX r s2n Percent
parseCurrency r = parseX r (replace (rg ",") "" >>> s2n) Currency
parseDateTime r = parseX r Date.fromString DateTime 

parseInterval r s = case split " - " s of
  (s1 : s2 : []) | test r s1 && test r s2 ->
    Interval <$> Date.fromString s1 <*> Date.fromString s2
  _ -> Nothing

-- analyzeStr :: {regexps} -> String -> JSemantic
analyzeStr rs s = (Text s) `fromMaybe`  (  parsePercent rs.percent s
                                      <|> parseCurrency rs.currency s
                                      <|> parseDateTime rs.date s
                                      <|> parseInterval rs.date s
                                      )
  -- TODO: date, time

toSemantic :: JSemanticOpts -> JsonPrim -> JSemantic
toSemantic o p = runJsonPrim p (const NA) Bool analyzeNum (analyzeStr o.regexps)

toSemanticDef :: JsonPrim -> JSemantic
toSemanticDef = toSemantic jSemanticOptsDefault

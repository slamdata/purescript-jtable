module Data.Json.JSemantic
  ( JSemantic(..)
  , toSemantic
  ) where

import Data.Argonaut.JCursor(JsonPrim(..), runJsonPrim)
import Data.Maybe
import Data.String.Regex (RegexFlags(..), regex, test)
import Data.String
import Data.Foldable
import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import Data.Tuple
import qualified Data.Date as Date
import Math


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
  show (Integral   n) = show n
  show (Fractional n) = show n
  show (Date       d) = show d
  show (DateTime   d) = show d
  show (Time       d) = show d
  show (Interval u v) = show u ++ " - " ++ show v
  show (Text       s) = s
  show (Bool       b) = show b
  show (Percent    n) = show n ++ "%"
  show (Currency   n) = "$" ++ show n
  show (NA)           = ""

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


noFlags :: RegexFlags
noFlags  = { global     : false
           , ignoreCase : false
           , multiline  : false
           , sticky     : false
           , unicode    : false }

percentRegex :: String -> Boolean
percentRegex = test $ regex """^-?\d+(\.\d+)?\%$""" noFlags

currencyRegex :: String -> Boolean
currencyRegex = test $ regex "^\\$?([0-9]{1,3},([0-9]{3},)*[0-9]{3}|[0-9]+)(.[0-9][0-9])?$" noFlags

-- somewhat like ISO8601
dateRegex :: String -> Boolean
dateRegex = test $ regex """^(\d{4})-(\d{2})-(\d{2})T(\d{2})\:(\d{2})\:(\d{2})[+-](\d{2})\:(\d{2})$""" noFlags

analyzeNum :: Number -> JSemantic
analyzeNum n | floor n == n = Integral n
analyzeNum n                = Fractional n

foreign import parsePercent """function parsePercent (s) {
  return s.replace("%", "") * 1}""" :: String -> Number

foreign import parseCurrency """function parseCurrency (s) {
  return (s.replace(/\,/g, "").replace(/\$/g, "")) * 1}""" :: String -> Number


analyzeStr :: String -> JSemantic
analyzeStr s = 
  if percentRegex s then Percent $ parsePercent s 
  else if currencyRegex s then Currency $ parseCurrency s
  else if dateRegex s then case Date.fromString s of
    (Just d) ->  DateTime d
    Nothing -> Text s -- shouldn't happen
  else 
    let sp = split " - " s in 
    if A.length sp == 2 && all dateRegex sp 
    then case Tuple (Date.fromString $ AU.head sp) 
                    (Date.fromString $ AU.head $ AU.tail sp) of
           (Tuple (Just u) (Just v)) -> Interval u v
           _ -> Text s -- shouldn't happen
    else Text s
  -- TODO: date, time

toSemantic :: JsonPrim -> JSemantic
toSemantic p = runJsonPrim p (const NA) Bool analyzeNum analyzeStr

module Data.Json.JTable.Internal
  ( renderJTableRaw, renderRows, renderThead, renderTbody, tsToRows, sortTree
  , Tree(..), tPath, tWidth, tHeight, tKids
  , Cell(..), cCursor, cWidth, cHeight, cJsonPrim
  , JPath(..), Table(..)
  , tFromJson, tMergeArray, widthOfPrimTuple, padTree
  , cFromJson, cMergeObj, mergeObjTuple
  , foldJsonP, toPrim, zipWithIndex, orelse, strcmp, _nattr, _cspan, _rspan
  ) where

import Math (max)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..), fst, snd, uncurry, zip)
import Data.String (joinWith, localeCompare)
import Data.Array (head, tail, length, null, singleton, snoc, concat, 
                   sortBy, nub, findIndex, updateAt, (!!), (..))
import qualified Data.StrMap as M
import Data.Foldable (foldl, any, all, mconcat, elem)
import Data.Traversable (for)
import Control.Bind ((=<<))
import Control.MonadPlus (guard)

import Data.Argonaut.Core (Json(..), JArray(..), JObject(..), foldJson, toObject)
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(..), downField, downIndex, 
                              primNull, primBool, primNum, primStr, primToJson)
import Data.Argonaut.Parser (jsonParser)
import Text.Smolder.HTML (thead, tbody, tr, th, td)
import Text.Smolder.Markup (Markup(..), Attribute(..), attribute, (!))

-- path of object keys, with array indices omitted
type JPath = [String]

-- rows of cells
type Table = [[Cell]]

-- header data
data Tree = T String JPath Number Number [Tree]
tLabel (T l _ _ _ _) = l
tPath (T _ p _ _ _) = p
tWidth (T _ _ w _ _) = w
tHeight (T _ _ _ h _) = h
tKids (T _ _ _ _ k) = k

instance showTree :: Show Tree where
  show (T l p w h k) = 
    joinWith " " ["(T", show l, show p, show w, show h, show k, ")"]

-- cell data
data Cell = C  JCursor Number Number JsonPrim
cCursor (C c _ _ _) = c
cWidth (C _ w _ _) = w
cHeight (C _ _ h _) = h
cJsonPrim (C _ _ _ j) = j

instance showCell :: Show Cell where
  show (C c w h j) = joinWith " " ["(C", show w, show h, show c, show j, ")"]


foldJsonP :: forall a. (JsonPrim -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
foldJsonP f = 
  foldJson (\_-> f primNull) (f <<< primBool) (f <<< primNum) (f <<< primStr)

toPrim :: Json -> Maybe JsonPrim
toPrim = foldJsonP Just (const Nothing) (const Nothing)

zipWithIndex :: forall a. [a] -> [Tuple a Number]
zipWithIndex xs = zip xs (0 .. ((length xs) - 1))

orelse :: forall a b. (a -> Maybe b) -> (a -> b) -> (a -> b)
orelse f g = fromMaybe <$> g <*> f 


-- maybe return the width of a tuple composed of primitive values
widthOfPrimTuple :: Number -> JPath -> [Json] -> Maybe Number
widthOfPrimTuple hS path ja =
  path !! 0 >>= \_-> ja !! 1 >>= \_-> let
    types = ja <#> foldJson (\_->0) (\_->1) (\_->2) (\_->3) (\_->4) (\_->4)
    not_same = length (nub types) /= 1
    all_prim = all ((/=) 4) types
    homoTup = length ja <= max 1 hS
    in guard (all_prim && (not_same || homoTup)) <#> \_-> length ja


-- add child to tree, unify if exists
tMergeArray :: Tree -> Tree -> Tree
tMergeArray (T l p w h k) nt@(T nl np nw nh nk) =
  let i = findIndex (tLabel >>> (== nl)) k in case k !! i of
    Just child_t@(T cl cp cw ch ck) -> case foldl tMergeArray child_t nk of 
      (T _ _ cw' ch' ck') -> let cw'' = max cw' nw
                                 w' = w - cw + cw''
                                 h' = max h (ch' + 1)
                                 k' = updateAt i (T cl cp cw'' ch' ck') k
                             in T l p w' h' k'
    Nothing -> let w' = if null k then nw else w+nw
                   h' = max h $ nh + 1
                   k' = snoc k nt
               in T l p w' h' k'

-- produce a tree of header data from json
tFromJson :: Number -> String -> JPath -> Json -> Tree
tFromJson hS label path = let
  prim = \jp -> T label path 1 0 []
  tuple = \ja -> widthOfPrimTuple hS path ja <#> \n -> T label path n 0 []
  array = \ja -> let
    ts = ja <#> tFromJson hS label path
    ks = ts >>= tKids
    t = foldl tMergeArray (T label path 0 0 []) ks
    tsw = case (nub $ ts <#> tWidth) of (tsw:[]) -> tsw
                                        _ -> 1
    w = max tsw (t # tWidth)
    in T label path w (t # tHeight) (t # tKids) 
  obj = \jo ->
    if M.isEmpty jo then T label path 1 0 []    
    else let k = M.toList jo <#> uncurry \l j -> tFromJson hS l (snoc path l) j
             w = k <#> tWidth # foldl (+) 0
             h = 1 + foldl max 0 (k <#> tHeight)
         in T label path w h k
  in foldJsonP prim (tuple `orelse` array) obj


-- merge table segments for each key of an object into one
cMergeObj :: [(Tuple Number Table)] -> Table
cMergeObj rss = let
  maxh = rss <#> snd <#> length # foldl max 0
  in (0 .. (maxh - 1)) <#> \n -> rss >>= uncurry \w rs -> let
    rnOr = (`fromMaybe` (rs !! n))
    in case rs of (r : []) -> if n == 0 then r <#> \(C c w h j) -> C c w maxh j
                                        else rnOr [] 
                  _        -> rnOr [C (JCursorTop) w 1 primNull]

-- maybe merge a tuple of objects into a table segment
mergeObjTuple ::Number -> Tree -> JCursor -> [Json] -> Maybe Table
mergeObjTuple hS t@(T l p w h k) c ja = do
  guard $ not $ null ja
  jos <- for ja toObject
  let keyss = jos <#> M.keys
  let all_keys = concat keyss
  guard $ (length all_keys) == (length $ nub all_keys)
  Just $ cMergeObj $ k <#> \(t'@(T l' p' w' _ _)) -> let 
    i = findIndex (elem l') keyss
    j = (primToJson primNull) `fromMaybe` (jos !! i >>= M.lookup l')
    in Tuple w' (cFromJson hS t' (c # downIndex i # downField l') j)

-- produce data table from json, according to header tree
cFromJson :: Number -> Tree -> JCursor -> Json -> Table
cFromJson hS t@(T l p w h k) c = let
  prim = \jp -> [[C c w 1 jp]]
  width = if h <= 0 && w > 1 then widthOfPrimTuple hS p else const Nothing
  primtup = \ja -> width ja <#> \_-> singleton $ (0 .. (w - 1)) <#> \i -> 
    C (downIndex i c) 1 1 $ primNull `fromMaybe` (ja !! i >>= toPrim)
  objtup = mergeObjTuple hS t c
  tuple = \ja -> case primtup ja of Nothing -> objtup ja
                                    m       -> m
  array = \ja -> zipWithIndex ja >>= uncurry \j i -> 
                   cFromJson hS t (downIndex i c) j
  obj = \jo -> if M.isEmpty jo then [[C c w 1 primNull]] 
               else cMergeObj $ k <#> \(t'@(T l' p' w' _ _)) -> let
                  j = (primToJson primNull) `fromMaybe` M.lookup l' jo
                  in Tuple w' (cFromJson hS t' (c # downField l') j)
  in foldJsonP prim (tuple `orelse` array) obj


-- render a grid from an array of arrays
renderRows :: forall a. (Markup -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> Markup
renderRows tr' cellf rows = mconcat $ do
  (Tuple row y) <- zipWithIndex rows
  return $ tr' $ mconcat $ do
    (Tuple cell x) <- zipWithIndex row
    return $ cellf y x cell

_nattr :: String -> Number -> Markup -> Markup
_nattr attr n m = if n > 1 then m ! (attribute attr $ show n) else m
_cspan = _nattr "colspan"
_rspan = _nattr "rowspan"

-- produce header rows from header tree
tsToRows :: [Tree] -> [[Tree]]
tsToRows ts = if null ts then [] else ts : tsToRows (ts >>= tKids)

renderThead :: (Markup -> Markup) -> (Tree -> Markup) -> Tree -> Markup
renderThead tr' thf (T l p w h k) =
  let rs i k = if null k then h - i else 1
      tdf' y x t@(T l p w h k) = thf t # _cspan w >>> (_rspan $ rs y k)
  in renderRows tr' tdf' $ tsToRows k

renderTbody :: (Markup -> Markup) -> (Cell -> Markup) -> Tree -> Table -> Markup
renderTbody tr' tdf t table =
  let tdf' y x cell@(C c w h j) = tdf cell # _cspan w >>> _rspan h
  in renderRows tr' tdf' table

-- sort header tree by ColumnOrdering
sortTree :: (String -> JPath -> String -> JPath -> Ordering) -> Tree -> Tree
sortTree ord (T l p w h k) = T l p w h $ 
  sortBy (\(T l1 p1 _ _ _) (T l2 p2 _ _ _) -> ord l1 p1 l2 p2) (k <#> sortTree ord)


strcmp :: String -> String -> Ordering
strcmp s1 s2 = compare (localeCompare s1 s2) 0


-- pad tall header cells from above
padTree :: Number -> Tree -> Tree
padTree maxh t@(T l p w h k) = 
  if not $ null k  then T l  p w h (k <#> padTree (maxh - 1))
  else if maxh > 0 then T "" p w 1 [padTree (maxh - 1) t]
                   else T l  p w 1 k

-- renderJTableRaw :: {...} -> Json -> Markup
renderJTableRaw o json = let
  rawT = tFromJson o.maxHomoTupSize "" [] json
  sortedT = sortTree o.columnOrdering rawT
  paddedT = sortedT # if o.insertHeaderCells then padTree (sortedT # tHeight) else id
  table = cFromJson o.maxHomoTupSize sortedT JCursorTop json
  in o.style.table $ do 
    thead $ renderThead o.style.tr o.style.th paddedT
    tbody $ renderTbody o.style.tr o.style.td sortedT table

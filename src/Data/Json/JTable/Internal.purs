module Data.Json.JTable.Internal
  ( renderJTableRaw, renderRows, renderThead, renderTbody, tsToRows, sortTree
  , Tree(..), tPath, tWidth, tHeight, tKids
  , Cell(..), cCursor, cWidth, cHeight, cJsonPrim
  , JPath(..), Table(..)
  , tFromJson, tMergeArray, widthOfPrimTuple, insertHeaderCells
  , cFromJson, cMergeObj, mergeObjTuple
  , foldJsonP, toPrim, zipWithIndex, orelse, strcmp, _nattr, _cspan, _rspan
  ) where

import Math (max)
import Data.Either
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple
import Data.String (joinWith, localeCompare)
import Data.Array
import Data.Foldable
import qualified Data.Array.Unsafe as AU
import qualified Data.StrMap as M
import Data.Foldable (foldl, any, all, mconcat, elem)
import Data.Traversable (for)
import Control.Alt
import Control.Apply
import Control.Bind
import Control.Monad
import Control.MonadPlus

import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Data.Argonaut.Parser (jsonParser)
import Text.Smolder.HTML (thead, tbody, tr, th, td)
import Text.Smolder.Markup (Markup(..), Attribute(..), attribute, (!))

-- path of object keys, with array indices omitted
type JPath = [String]

-- rows of cells
type Table = [[Cell]]

-- header data
data Tree = T JPath Number Number [Tree]
tPath (T p _ _ _) = p
tWidth (T _ w _ _) = w
tHeight (T _ _ h _) = h
tKids (T _ _ _ k) = k

instance showTree :: Show Tree where
  show (T p w h k) = joinWith " " ["(T", show p, show w, show h, show k, ")"]

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

zipWithIndex xs = zip xs (0 .. ((length xs) - 1))

orelse f g x = case f x of Just v -> v 
                           _      -> g x


-- maybe return the width of a tuple composed of primitive values
widthOfPrimTuple :: Number -> JPath -> [Json] -> Maybe Number
widthOfPrimTuple hS path ja =
  path !! 0 *> ja !! 1 *> let
    types = ja <#> foldJson (\_->0) (\_->1) (\_->2) (\_->3) (\_->4) (\_->4)
    not_same = length (nub types) /= 1
    all_prim = all ((/=) 4) types
    homoTup = length ja <= max 1 hS
    in guard (all_prim && (not_same || homoTup)) <#> \_-> length ja


-- add child to tree, unify if exists
tMergeArray :: Tree -> Tree -> Tree
tMergeArray (T p w h k) nt@(T np nw nh nk) =
  let i = findIndex (\n -> last np == last (n # tPath)) k in case k !! i of
    Just child_t@(T cp cw ch ck) -> case foldl tMergeArray child_t nk of 
                 (T _ cw' ch' ck') -> let cw'' = max cw' nw
                                          w' = w - cw + cw''
                                          h' = max h (ch' + 1)
                                          k' = updateAt i (T cp cw'' ch' ck') k
                                      in T p w' h' k'
    Nothing -> let w' = if null k then nw else w+nw
                   h' = max h $ nh + 1
                   k' = snoc k nt
               in T p w' h' k'

-- produce a tree of header data from json
tFromJson :: Number -> JPath -> Json -> Tree
tFromJson hS path = let
  prim = \jp -> T path 1 0 []
  tuple = \ja -> widthOfPrimTuple hS path ja <#> \n -> T path n 0 []
  array = \ja -> let
    ts = ja <#> tFromJson hS path
    ks = ts >>= tKids
    t = foldl tMergeArray (T path 0 0 []) ks
    tsw = case (nub $ ts <#> tWidth) of (tsw:[]) -> tsw
                                        _ -> 1
    w = max tsw (t # tWidth)
    in T path w (t # tHeight) (t # tKids) 
  obj = \jo ->
    if M.isEmpty jo then T path 1 0 []    
    else let k = M.toList jo <#> uncurry \l j -> tFromJson hS (snoc path l) j
             w = k <#> tWidth # foldl (+) 0
             h = 1 + foldl max 0 (k <#> tHeight)
         in T path w h k
  in foldJsonP prim (tuple `orelse` array) obj


-- merge table segments for each key of an object into one
cMergeObj :: [(Tuple Number Table)] -> Table
cMergeObj rss = let
  maxh = rss <#> snd <#> length # foldl max 0
  in (0 .. (maxh - 1)) <#> \n -> rss >>= uncurry \w rs -> let
    rnOr = flip fromMaybe (rs !! n)
    in case rs of (r : []) -> if n == 0 then r <#> \(C c w h j) -> C c w maxh j
                                        else rnOr [] 
                  _        -> rnOr [C (JCursorTop) w 1 primNull]

-- maybe merge a tuple of objects into a table segment
mergeObjTuple ::Number -> Tree -> JCursor -> [Json] -> Maybe Table
mergeObjTuple hS t@(T p w h k) c ja = head ja *> do
  jos <- for ja toObject
  let keyss = jos <#> M.keys
  let all_keys = concat keyss
  guard ((length all_keys) == (length $ nub all_keys)) <#> \_->
    cMergeObj $ k <#> \(t'@(T p' w' _ _)) -> let 
      label = AU.last p'
      i = findIndex (elem label) keyss
      j = (primToJson primNull) `fromMaybe` (jos !! i >>= M.lookup label)
      in Tuple w' (cFromJson hS t' (downField label $ downIndex i c) j)

-- produce data table from json, according to header tree
cFromJson :: Number -> Tree -> JCursor -> Json -> Table
cFromJson hS t@(T p w h k) c = let
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
               else cMergeObj $ k <#> \(t'@(T p' w' _ _)) -> let
                  label = AU.last p'
                  j = (primToJson primNull) `fromMaybe` M.lookup label jo
                  in Tuple w' (cFromJson hS t' (downField label c) j)
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
renderThead tr' thf (T p w h k) =
  let rs i k = if null k then h - i else 1
      tdf' y x t@(T p w h k) = thf t # _cspan w >>> (_rspan $ rs y k)
  in renderRows tr' tdf' $ tsToRows k

renderTbody :: (Markup -> Markup) -> (Cell -> Markup) -> Tree -> Table -> Markup
renderTbody tr' tdf t table =
  let tdf' y x cell@(C c w h j) = tdf cell # _cspan w >>> _rspan h
  in renderRows tr' tdf' table

-- sort header tree by ColumnOrdering
sortTree :: (JPath -> JPath -> Ordering) -> Tree -> Tree
sortTree ord (T p w h k) = T p w h $
  sortBy (\t1 t2 -> ord (t1 # tPath) (t2 # tPath)) (k <#> sortTree ord)


strcmp :: String -> String -> Ordering
strcmp s1 s2 = compare (localeCompare s1 s2) 0


-- pad tall header cells from above
insertHeaderCells :: Number -> Tree -> Tree
insertHeaderCells maxh t@(T p w h k) = 
  if null k then if maxh > 0 then T [] w 1 [insertHeaderCells (maxh - 1) t]
                             else T p w 1 k
            else T p w h (k <#> insertHeaderCells (maxh - 1))

-- renderJTableRaw :: {...} -> Json
renderJTableRaw o json = 
  o.style.table $ do 
    let t  = sortTree o.columnOrdering $ tFromJson o.maxHomoTupSize [] json
    let t' = if o.insertHeaderCells then insertHeaderCells (t # tHeight) t 
                                    else t
    let table = cFromJson o.maxHomoTupSize t JCursorTop json
    thead $ renderThead o.style.tr o.style.th t'
    tbody $ renderTbody o.style.tr o.style.td t table

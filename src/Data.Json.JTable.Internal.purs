module Data.Json.JTable.Internal
  ( renderJTableRaw, renderRows, renderThead, renderTbody, tsToRows, sortTree
  , Tree(..), tPath, tWidth, tHeight, tKids
  , Cell(..), cCursor, cWidth, cHeight, cJsonPrim
  , JPath(..), Table(..)
  , tFromJson, tMergeArray, widthOfPrimTuple, insertHeaderCells
  , cFromJson, cMergeObj, mergeObjTuple
  , _cN, toPrim, strcmp, _nattr, _cspan, _rspan
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


foreign import jnull "var jnull = null;" :: Json

_cN = const primNull
toPrim = (foldJson _cN primBool primNum primStr _cN _cN) :: Json -> JsonPrim
zipWithIndex xs = zip xs (0 .. length xs - 1) 

justIf :: forall a. Boolean -> a -> Maybe a
justIf b x = if b then Just x else Nothing


-- maybe return the width of a tuple composed of primitive values
widthOfPrimTuple :: JPath -> [Json] -> Maybe Number
widthOfPrimTuple path ja = 
  path !! 0 *> ja !! 1 *> let
    types = ja <#> foldJson (\_->0) (\_->1) (\_->2) (\_->3) (\_->4) (\_->4)
    not_same = length (nub types) /= 1
    all_prim = all ((/=) 4) types
    in justIf (all_prim && (not_same || length ja == 2)) $ length ja


-- add child to tree, unify if exists
tMergeArray :: Tree -> Tree -> Tree
tMergeArray (T p w h k) nt@(T np nw nh nk) =
  let i = findIndex (\n -> last np == last (n # tPath)) k in case k !! i of
    Just child_t@(T cp cw ch ck) -> case foldl tMergeArray child_t nk of 
                 (T _ cw' ch' ck') -> let w' = w - cw + cw'
                                          h' = max h (ch' + 1)
                                          k' = updateAt i (T cp cw' ch' ck') k
                                      in T p w' h' k'
    Nothing -> let w' = if null k then nw else w+nw
                   h' = max h $ nh + 1
                   k' = snoc k nt
               in T p w' h' k'

-- produce a tree of header data from json
tFromJson :: JPath -> Json -> Tree
tFromJson path json = let
  obj = toObject json <#> \jo ->
    if M.isEmpty jo then T path 1 0 []    
    else let k = M.toList jo <#> uncurry \l j -> tFromJson (snoc path l) j
             w = k <#> tWidth # foldl (+) 0
             h = 1 + foldl max 0 (k <#> tHeight)
         in T path w h k
  m = (>>=) (toArray json)
  tuple = m \a -> (widthOfPrimTuple path a) <#> \n -> T path n 0 []
  array = m \a -> let
    ts = a <#> tFromJson path
    t' = foldl tMergeArray (T path 0 0 []) (ts >>= tKids)
    w' = t' # tWidth
    mw = ((head $ nub (ts <#> tWidth)) <#> max w') <|> (Just $ max 1 w')
    in mw <#> \w -> T path w (t' # tHeight) (t' # tKids) 
  prim = T path 1 0 []
  in prim `fromMaybe` (obj <|> tuple <|> array)


-- merge table segments for each key of an object into one
cMergeObj :: [(Tuple Number Table)] -> Table
cMergeObj rss = let
  maxh = rss <#> snd <#> length # foldl max 0
  in (0 .. maxh-1) <#> \n -> rss >>= uncurry \w rs ->
    if length rs /= 1 
    then [C (JCursorTop) w 1 primNull] `fromMaybe` (rs !! n)
    else if n == 0 
         then AU.head rs <#> \(C c w h j) -> C c w maxh j
         else [] `fromMaybe` (rs !! n)

-- maybe merge a tuple of objects into a table segment
mergeObjTuple ::Tree -> JCursor -> [Json] -> Maybe Table
mergeObjTuple t@(T p w h k) c ja = head ja *> do
  jos <- for ja toObject
  let keyss = jos <#> M.keys
  let all_keys = concat keyss
  justIf ((length all_keys) == (length $ nub all_keys)) $ 
    cMergeObj $ k <#> \(t'@(T p' w' _ _)) -> let 
      label = AU.last p'
      i = findIndex (elem label) keyss
      j = jnull `fromMaybe` (jos !! i >>= M.lookup label)
      in Tuple w' (cFromJson t' (downField label $ downIndex i c) j)

-- produce data table from json, according to header tree
cFromJson :: Tree -> JCursor -> Json -> Table
cFromJson t@(T p w h k) c json = let
  obj = toObject json <#> \jo ->
    if M.isEmpty jo then [[C c w 1 primNull]] 
    else cMergeObj $ do
      t'@(T p' w' _ _) <- k
      let label = AU.last p'
      let j = jnull `fromMaybe` M.lookup label jo
      return $ Tuple w' (cFromJson t' (downField label c) j)
  ma = toArray json
  obtup = ma >>= mergeObjTuple t c
  width = ma >>= widthOfPrimTuple p >>= justIf (h <= 0 && w > 1)
  tuple = ma <#> \a -> singleton $ (0 .. w-1) <#> \i ->
    C (downIndex i c) 1 1 $ toPrim $ jnull `fromMaybe` (a !! i)
  array = ma <#> \a -> zipWithIndex a >>= uncurry
                  \j i -> cFromJson t (downIndex i c) j
  prim = [[C c w 1 $ toPrim json]]
  in prim `fromMaybe` (obj <|> obtup <|> (width *> tuple) <|> array)


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

renderTbody :: (Markup -> Markup) -> (Cell -> Markup) -> Tree -> Json -> Markup
renderTbody tr' tdf t json =
  let tdf' y x cell@(C c w h j) = tdf cell # _cspan w >>> _rspan h
  in renderRows tr' tdf' $ cFromJson t JCursorTop json

-- sort header tree by ColumnOrdering
sortTree :: (JPath -> JPath -> Ordering) -> Tree -> Tree
sortTree ord (T p w h k) = T p w h $
  sortBy (\t1 t2 -> ord (t1 # tPath) (t2 # tPath)) (k <#> sortTree ord)


strcmp :: String -> String -> Ordering
strcmp s1 s2 = compare (localeCompare s1 s2) 0


-- pad tall header cells from above
insertHeaderCells :: Number -> Tree -> Tree
insertHeaderCells maxh t@(T p w h k) = 
  if null k 
  then if maxh > 0 then T [] w 1 [insertHeaderCells (maxh-1) t]
                   else T p w 1 k
  else T p w h (k <#> insertHeaderCells (maxh - 1))


-- renderJTableRaw :: {...} -> Json
renderJTableRaw o json = 
  o.style.table $ do 
    let t  = sortTree o.columnOrdering $ tFromJson [] json
    let t' = if o.insertHeaderCells then insertHeaderCells (t # tHeight) t 
                                    else t
    thead $ renderThead o.style.tr o.style.th t'
    tbody $ renderTbody o.style.tr o.style.td t json

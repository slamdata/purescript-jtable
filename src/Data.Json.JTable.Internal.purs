module Data.Json.JTable.Internal
  ( renderJTableRaw, renderRows, renderThead, renderTbody, tsToRows, sortTree
  , Tree(..), tPath, tWidth, tHeight, tKids
  , Cell(..), cCursor, cWidth, cHeight, cJsonPrim
  , JPath(..), Table(..)
  , tFromJson, tMergeArray, widthOfPrimTuple, insertHeaderCells
  , cFromJson, cMergeObj, mergeObjTuple
  , _cN, toPrim, strcmp, _nattr, _cspan, _rspan
  ) where

import Data.Either
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple
import Data.String (joinWith, localeCompare)
import Data.Array
import qualified Data.Array.Unsafe as AU
import qualified Data.StrMap as M
import Data.Foldable (foldl, any, all, mconcat)
import Math (max)

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


-- maybe return the width of a tuple composed of primitive values
widthOfPrimTuple :: JPath -> [Json] -> Maybe Number
widthOfPrimTuple path ja = 
  if null path || length ja <= 1 then Nothing else let
    f = foldJson (const 0) (const 1) (const 2) (const 3) (const 4) (const 5)
    types = ja <#> f
    has_arr_or_obj = any (\x -> x >= 4) types
    all_eq = all ((==) $ AU.head types) types
    in if has_arr_or_obj || (all_eq && length ja > 2) then Nothing 
                                                      else Just $ length ja

-- add child to tree, unify if exists
tMergeArray :: Tree -> Tree -> Tree
tMergeArray (T p w h k) t1@(T p1 w1 h1 k1) =
  let i = findIndex (\n -> last p1 == last (n # tPath)) k in case k !! i of
    Just t2@(T p2 w2 h2 k2) -> case foldl tMergeArray t2 k1 of 
      (T _ w2' h2' k2') -> let k' = updateAt i (T p2 w2' h2' k2') k
                               w' = w - w2 + w2'
                               h' = max h (h2' + 1)
                           in T p w' h' k'
    Nothing -> let w' = if null k then w1 else w+w1
                   h' = max h (h1 + 1)
                   k' = snoc k (T p1 w1 h1 k1)
               in T p w' h' k'

-- produce a tree of header data from json
tFromJson :: JPath -> Json -> Tree
tFromJson path json =
  case json # toObject of -- object
    Just jo -> if M.isEmpty jo then T path 1 0 [] else let
               k = map (\(Tuple l j) -> tFromJson (snoc path l) j) (M.toList jo)
               w = foldl (+) 0 (k <#> tWidth)
               h = 1 + foldl max 0 (k <#> tHeight)
               in T path w h k
    Nothing -> case json # toArray of
      Nothing -> T path 1 0 [] -- primitive
      Just ja -> case widthOfPrimTuple path ja of
        Just n -> T path n 0 [] -- tuple
        Nothing -> -- array
          let ts = ja <#> tFromJson path
              tk = ts >>= tKids
              t' = foldl tMergeArray (T path 0 0 []) tk
              tsws = ts <#> tWidth
              w = if (not $ null ts) && (all ((==) (AU.head tsws)) tsws)
                  then max (AU.head tsws) (t' # tWidth)
                  else max 1 (t' # tWidth)
          in T path w (t' # tHeight) (t' # tKids)


-- merge table segments for each key of an object into one
cMergeObj :: [(Tuple Number Table)] -> Table
cMergeObj rss = do
  let maxh = foldl (\n (Tuple w l) -> max n $ length l) 0 rss
  n <- 0 .. maxh-1
  return $ concat $ do
    (Tuple w rs)<- rss
    return $  if length rs == 1 
              then if n == 0 then (\(C c w h j) -> C c w maxh j) `map` (AU.head rs)
                             else fromMaybe [] $ rs !! n 
              else fromMaybe [C (JCursorTop) w 1 primNull] $ rs !! n

-- maybe merge a tuple of objects into a table segment
mergeObjTuple ::Tree -> JCursor -> [Json] -> Maybe Table
mergeObjTuple t@(T p w h k) c ja = 
  let joms = ja <#> toObject
  in if (null ja) || (not $ all isJust joms) then Nothing
     else let jos = catMaybes joms
              keyss = jos <#> M.keys
              all_keys = concat keyss
          in if length (nub all_keys) /= length all_keys
             then Nothing
             else Just $ cMergeObj $ do 
               t'@(T p' w' _ _) <- k
               let label = AU.last p'
               let i = findIndex (\ks -> elemIndex label ks > -1) keyss
               let jo = fromMaybe M.empty $ jos !! i
               let j = fromMaybe jnull $ M.lookup label jo
               return $ (Tuple w' $ cFromJson t' (downField label $ downIndex i c) j)

-- produce data table from json, according to header tree
cFromJson :: Tree -> JCursor -> Json -> Table
cFromJson t@(T p w h k) c json = 
  case json # toObject of 
    Just jo -> if M.isEmpty jo then [[C c w 1 primNull]] else
      cMergeObj $ do -- object
        t'@(T p' w' _ _) <- k
        let label = AU.last p'
        let j = fromMaybe jnull $ M.lookup label jo
        return $ (Tuple w' $ cFromJson t' (downField label c) j)
    Nothing -> case json # toArray of
      Nothing -> [[C c w 1 $ toPrim json]] -- primitive
      Just ja -> case mergeObjTuple t c ja of
        Just css -> css -- tuple of objects
        Nothing ->
          if h <= 0 && w > 1 && (isJust $ widthOfPrimTuple p ja)
          then singleton $ do -- tuple
            i <- 0 .. w-1
            let j = fromMaybe jnull $ ja !! i
            return $ C (downIndex i c) 1 1 $ toPrim j
          else concat $ do -- array
            (Tuple j i)<- ja `zip` (0 .. length ja)
            return $ cFromJson t (downIndex i c) j

-- render a grid from an array of arrays
renderRows :: forall a. (Markup -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> Markup
renderRows tr' cellf rows = mconcat $ do
  (Tuple row y) <- rows `zip` (0 .. length rows)
  return $ tr' $ mconcat $ do
    (Tuple cell x) <- row `zip` (0 .. length row)
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
sortTree ord (T p w h k) = T p w h ( 
  sortBy (\t1 t2 -> ord (t1 # tPath) (t2 # tPath)) (k <#> sortTree ord))


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

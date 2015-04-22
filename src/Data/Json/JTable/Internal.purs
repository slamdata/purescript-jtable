module Data.Json.JTable.Internal
  ( renderJTableRaw, renderRows, renderThead, renderTbody, tsToRows, sortTree
  , Tree(..), Cell(..)
  , JPath(), Table(), Markup(), TableStyle(), ColumnOrdering(), JTableOpts()
  , tFromJson, tMergeArray, widthOfPrimTuple, padTree
  , cFromJson, cMergeObj, mergeObjTuple
  , foldJsonP, toPrim, zipWithIndex, strcmp
  ) where

import Math (max)

import Data.Void
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (Tuple(..), fst, snd, uncurry, zip)
import Data.String (joinWith, localeCompare)
import Data.Array (head, tail, length, null, singleton, snoc, concat, 
                   sortBy, nub, findIndex, updateAt, range, (..), (!!))
import qualified Data.StrMap as M
import Data.Foldable (foldl, any, all, mconcat, elem)
import Data.Traversable (for)
import Control.Bind ((=<<))
import Control.MonadPlus (guard)

import Data.Argonaut.Core (Json(..), JArray(..), JObject(..), foldJson, toObject)
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(..), downField, downIndex, 
                              primNull, primBool, primNum, primStr, primToJson)
import Data.Argonaut.Parser (jsonParser)

import qualified Halogen.HTML as H

-- path of object keys, with array indices omitted
type JPath = [String]

-- rows of cells
type Table = [[Cell]]

-- header data
newtype Tree = Tree
  { label :: String
  , path :: JPath
  , width :: Number
  , height :: Number
  , kids :: [Tree]
  }
  
runTree :: Tree -> 
  { label :: String
  , path :: JPath
  , width :: Number
  , height :: Number
  , kids :: [Tree]
  }
runTree (Tree t) = t

-- cell data
newtype Cell = Cell
  { cursor :: JCursor
  , width :: Number
  , height :: Number
  , json :: JsonPrim
  }
  
runCell :: Cell -> 
  { cursor :: JCursor
  , width :: Number
  , height :: Number
  , json :: JsonPrim
  }
runCell (Cell c) = c

type Markup = H.HTML Void Void

type TableStyle =
  { table :: [Markup] -> Markup
  , tr    :: [Markup] -> Markup
  , th    :: String -> JPath -> Number -> Number -> Markup
  , td    :: JCursor -> JsonPrim -> Number -> Number -> Markup
  }
  
type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering

type JTableOpts = 
  { style :: TableStyle
  , columnOrdering :: ColumnOrdering
  , insertHeaderCells :: Boolean
  , maxTupleSize :: Number
  }

foldJsonP :: forall a. (JsonPrim -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
foldJsonP f = 
  foldJson (\_-> f primNull) (f <<< primBool) (f <<< primNum) (f <<< primStr)

toPrim :: Json -> Maybe JsonPrim
toPrim = foldJsonP Just (const Nothing) (const Nothing)

zipWithIndex :: forall a. [a] -> [Tuple a Number]
zipWithIndex xs = zip xs (0 .. ((length xs) - 1))

-- | Maybe return the width of a tuple composed of primitive values
widthOfPrimTuple :: Number -> JPath -> [Json] -> Maybe Number
widthOfPrimTuple maxTupleSize path ja = do
  path !! 0
  ja !! 1
  for ja toPrim
  if length ja <= maxTupleSize 
    then Just (length ja) 
    else Nothing

-- | Add child to tree, unify if exists
tMergeArray :: Tree -> Tree -> Tree
tMergeArray (Tree t) (Tree nt) =
  let i = findIndex (runTree >>> _.label >>> (== (nt.label))) t.kids in case t.kids !! i of
    Just c@(Tree child) -> case foldl tMergeArray c nt.kids of 
      Tree child' -> 
        let cw'' = max child'.width if null nt.kids && not (null child'.kids) then 1 else nt.width
            w' = t.width - child.width + cw''
            h' = max t.height (child'.height + 1)
            k' = updateAt i (Tree { label: child.label, path: child.path, width: cw'', height: child'.height, kids: child'.kids }) t.kids
        in Tree { label: t.label, path: t.path, width: w', height: h', kids: k' }
    Nothing -> let w' = if null t.kids then nt.width else t.width + nt.width
                   h' = max t.height (nt.height + 1)
                   k' = snoc t.kids (Tree nt)
               in Tree { label: t.label, path: t.path, width: w', height: h', kids: k' }

-- | Produce a tree of header data from json
tFromJson :: Number -> String -> JPath -> Json -> Tree
tFromJson maxTupleSize label path = 
  let prim jp = Tree { label: label, path: path, width: 1, height: 0, kids: [] }
      tuple ja = widthOfPrimTuple maxTupleSize path ja <#> \n -> Tree { label: label, path: path, width: n, height: 0, kids: [] }
      array ja = let ts = ja <#> tFromJson maxTupleSize label path
                     t = runTree $ foldl tMergeArray (Tree { label: label, path: path, width: 0, height: 0, kids: [] }) (ts >>= runTree >>> _.kids)
                     w = max t.width w'
                     w' = case (nub $ ts <#> runTree >>> _.width) of 
                            [tsw] -> tsw
                            _     -> 1
                 in Tree { label: label, path: path, width: w, height: t.height, kids: t.kids }
      obj jo =
        if M.isEmpty jo then Tree { label: label, path: path, width: 1, height: 0, kids: [] }
        else let k = M.toList jo <#> uncurry \l j -> tFromJson maxTupleSize l (snoc path l) j
                 w = k <#> runTree >>> _.width # foldl (+) 0
                 h = 1 + foldl max 0 (k <#> runTree >>> _.height)
             in Tree { label: label, path: path, width: w, height: h, kids: k }
  in foldJsonP prim (fromMaybe <$> array <*> tuple) obj

-- | Merge table segments for each key of an object into one
cMergeObj :: [(Tuple Number Table)] -> Table
cMergeObj rss = let
  maxh = rss <#> snd <#> length # foldl max 0
  in (range 0 $ max 0 $ maxh - 1) <#> \n -> rss >>= uncurry \w rs -> let
    rnOr = (`fromMaybe` (rs !! n))
    in case rs of [r] -> if n == 0 then r <#> \(Cell c) -> Cell { cursor: c.cursor, width: c.width, height: maxh, json: c.json }
                                   else rnOr [] 
                  _   -> rnOr [Cell { cursor: JCursorTop, width: w, height: 1, json: primNull }]

-- | Maybe merge a tuple of objects into a table segment
mergeObjTuple :: Number -> Tree -> JCursor -> [Json] -> Maybe Table
mergeObjTuple maxTupleSize (Tree t) c ja = do
  head ja
  jos <- for ja toObject
  let keyss = jos <#> M.keys
  let all_keys = concat keyss
  guard $ (length all_keys) == (length $ nub all_keys)
  Just $ cMergeObj $ t.kids <#> \(Tree t') -> let 
    i = findIndex (elem t'.label) keyss
    j = primToJson primNull `fromMaybe` (jos !! i >>= M.lookup t'.label)
    in Tuple t'.width (cFromJson maxTupleSize (Tree t') (c # downIndex i # downField t'.label) j)

-- | Produce data table from json, according to header tree
cFromJson :: Number -> Tree -> JCursor -> Json -> Table
cFromJson maxTupleSize (Tree t) c = let
  prim jp = [[Cell { cursor: c, width: t.width, height: 1, json: jp }]]
  width = if null t.kids && t.width > 1 then widthOfPrimTuple maxTupleSize t.path else const Nothing
  primtup ja = width ja <#> \_ -> singleton $ (0 .. (t.width - 1)) 
                        <#> \i -> Cell { cursor: downIndex i c, width: 1, height: 1, json: primNull `fromMaybe` (ja !! i >>= toPrim) }
  objtup = mergeObjTuple maxTupleSize (Tree t) c
  tuple ja = case primtup ja of Nothing -> objtup ja
                                m       -> m
  array ja = zipWithIndex ja >>= uncurry \j i -> 
                   cFromJson maxTupleSize (Tree t) (downIndex i c) j
  obj jo = if M.isEmpty jo 
             then [[Cell { cursor: c, width: t.width, height: 1, json: primNull }]] 
             else cMergeObj $ t.kids <#> \(Tree t') -> 
               let j = (primToJson primNull) `fromMaybe` M.lookup t'.label jo
               in Tuple t'.width (cFromJson maxTupleSize (Tree t') (c # downField t'.label) j)
  in foldJsonP prim (fromMaybe <$> array <*> tuple) obj

-- | Render a grid from an array of arrays
renderRows :: forall a. ([Markup] -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> [Markup]
renderRows tr cellf rows = do
  Tuple row y <- zipWithIndex rows
  return $ tr do
    Tuple cell x <- zipWithIndex row
    return $ cellf y x cell

-- | Produce header rows from header tree
tsToRows :: [Tree] -> [[Tree]]
tsToRows ts = if null ts then [] else ts : tsToRows (ts >>= runTree >>> _.kids)

renderThead :: ([Markup] -> Markup) -> (String -> JPath -> Number -> Number -> Markup) -> Tree -> Markup
renderThead tr thf (Tree t) =
  let rs i k = if null k then t.height - i else 1
      tdf' y x (Tree t) = thf t.label t.path t.width (rs y t.kids)
  in H.thead_ (renderRows tr tdf' $ tsToRows t.kids)

renderTbody :: ([Markup] -> Markup) -> (JCursor -> JsonPrim -> Number -> Number -> Markup) -> Tree -> Table -> Markup
renderTbody tr tdf (Tree t) table =
  let tdf' y x (Cell c) = tdf c.cursor c.json c.width c.height
  in H.tbody_ (renderRows tr tdf' table)

-- | Sort header tree by ColumnOrdering
sortTree :: ColumnOrdering -> Tree -> Tree
sortTree ord (Tree t) = Tree $ t { kids = _ } $ sortBy (\(Tree t1) (Tree t2) -> ord t1.label t1.path t2.label t2.path) (t.kids <#> sortTree ord)

strcmp :: String -> String -> Ordering
strcmp s1 s2 = compare (localeCompare s1 s2) 0

-- | Pad tall header cells from above
padTree :: Number -> Tree -> Tree
padTree maxh (Tree t) = 
  if not $ null t.kids 
    then Tree (t { kids = t.kids <#> padTree (maxh - 1) })
    else if maxh > 0 then Tree { label: "", path: t.path, width: t.width, height: 1, kids: [padTree (maxh - 1) (Tree t)] }
                     else Tree (t { height = 1 })

renderJTableRaw :: JTableOpts -> Json -> Markup
renderJTableRaw o json = 
  let
    rawT = tFromJson o.maxTupleSize "" [] json
    sortedT = sortTree o.columnOrdering rawT
    paddedT = sortedT # if o.insertHeaderCells then padTree (runTree sortedT).height else id
    table = cFromJson o.maxTupleSize sortedT JCursorTop json
  in o.style.table
    [ renderThead o.style.tr o.style.th paddedT
    , renderTbody o.style.tr o.style.td sortedT table
    ]
module Data.Json.JTable.Internal (
  renderJTableRaw, JTableOpts(), ColumnOrdering(), Markup(), TableStyle(), JPath()
  ) where

import Prelude

import Data.Void
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), snd)
import Data.Foldable (foldl, elem)
import Data.Traversable (for)
import Control.MonadPlus (guard)
import Data.List (List(..), range, length, zipWith, null, fromList, snoc, head, (!!), catMaybes, singleton, concat, nub, findIndex, updateAt, sortBy, toList)
import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Data.Monoid (mempty)
import Control.Alt ((<|>))

import qualified Data.StrMap as M
import qualified Data.Array as A
import qualified Halogen.HTML as H

max :: forall a. (Ord a) => a -> a -> a
max a b = if a > b then a else b

-- path of object keys, with array indices omitted
type JPath = List String

-- rows of cells
type Table = List (List Cell)

-- header data
type TreeRec = 
  { label :: String
  , path :: JPath
  , width :: Int
  , height :: Int
  , children :: List Tree
  }
newtype Tree = Tree TreeRec
  
runTree :: Tree -> TreeRec
runTree (Tree t) = t

-- cell data
type CellRec =
  { cursor :: JCursor
  , width :: Int
  , height :: Int
  , json :: JsonPrim
  }
newtype Cell = Cell CellRec

  
runCell :: Cell -> CellRec
runCell (Cell c) = c

type Markup = H.HTML Void

type TableStyle =
  { table :: Array Markup -> Markup
  , tr    :: Array Markup -> Markup
  , th    :: String -> JPath -> Int -> Int -> Markup
  , td    :: JCursor -> JsonPrim -> Int -> Int -> Markup
  }
  
type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering

type JTableOpts = 
  { style :: TableStyle
  , columnOrdering :: ColumnOrdering
  , insertHeaderCells :: Boolean
  , maxTupleSize :: Int
  }

foldJsonPrim :: forall a. (JsonPrim -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
foldJsonPrim f = 
  foldJson (\_-> f primNull) (f <<< primBool) (f <<< primNum) (f <<< primStr)

toPrim :: Json -> Maybe JsonPrim
toPrim = foldJsonPrim Just (const Nothing) (const Nothing)

enumerate :: forall a. List a -> List (Tuple a Int)
enumerate xs = zipWith Tuple xs (range zero $ (length xs - one))

renderJTableRaw :: JTableOpts -> Json -> Markup
renderJTableRaw o json =
  o.style.table [ renderThead o.style.tr o.style.th padded
                , renderTBody o.style.tr o.style.td sorted table
                ]
  where
  padded :: Tree 
  padded = if o.insertHeaderCells
           then (padTree (runTree sorted).height) $ sorted
           else sorted
  sorted :: Tree
  sorted = sortTree o.columnOrdering raw

  raw :: Tree
  raw = treeFromJson o.maxTupleSize "" Nil json

  table :: Table
  table = mkTable o.maxTupleSize sorted JCursorTop json


padTree :: Int -> Tree -> Tree
padTree maxh tree@(Tree t) =
  if not $ null t.children
  then Tree $ t { children = padTree (maxh - one) <$> t.children }
  else if maxh < 1
       then Tree $ t{ height = one }
       else Tree $ t{ label = mempty
                    , height = one
                    , children = singleton $ padTree (maxh - one) (Tree t)
                    } 

sortTree :: ColumnOrdering -> Tree -> Tree
sortTree ord (Tree t) =
  Tree $ t {children = sortBy sortFn $ (sortTree ord <$> t.children)}
  where
  sortFn :: Tree -> Tree -> Ordering
  sortFn (Tree t1) (Tree t2) =
    ord t1.label t1.path t2.label t2.path

renderRows :: forall a. (Array Markup -> Markup) ->
              (Int -> Int -> a -> Markup) ->
              List (List a) -> Array Markup
renderRows tr cellf rows = fromList do
  Tuple row y <- fromList $ enumerate rows
  return $ tr do
    Tuple cell x <- fromList $ enumerate row
    return $ cellf y x cell

tablesToRows :: List Tree -> List (List Tree)
tablesToRows ts =
  if null ts
  then Nil
  else Cons ts (tablesToRows (ts >>= runTree >>> _.children))

renderThead :: (Array Markup -> Markup) ->
               (String -> JPath -> Int -> Int -> Markup) ->
               Tree -> Markup
renderThead tr thf (Tree t) =
  H.thead_ (renderRows tr tdf' $ tablesToRows t.children)
  where
  tdf' :: Int -> Int -> Tree -> Markup
  tdf' y x (Tree t) = thf t.label t.path t.width (height y t.children)

  height :: Int -> List Tree -> Int
  height i k =
    if null k
    then t.height - i
    else one

renderTBody :: (Array Markup -> Markup) ->
               (JCursor -> JsonPrim -> Int -> Int -> Markup) ->
               Tree -> Table -> Markup
renderTBody tr tdf (Tree t) table =
  H.tbody_ (renderRows tr tdf' table)
  where
  tdf' :: Int -> Int -> Cell -> Markup
  tdf' _ _ (Cell c) = tdf c.cursor c.json c.width c.height

treeFromJson :: Int -> String -> JPath -> Json -> Tree
treeFromJson maxTupleSize label path =
  foldJsonPrim (const prim) array obj
  where
  tree :: TreeRec
  tree = { label: label
         , path: path
         , width: one
         , height: zero
         , children: mempty
         } 
   
  prim :: Tree
  prim = Tree tree

  array :: JArray -> Tree
  array ja = fromMaybe (jarr ja) $ tuple ja

  tuple :: JArray -> Maybe Tree
  tuple ja =
    (Tree <<< tree { width = _ }) <$> 
    widthOfPrimTuple maxTupleSize path ja

  jarr :: JArray -> Tree
  jarr ja =
    Tree $ tree { width = width, height = t.height, children = t.children }
    where
    width :: Int
    width = max t.width childrenWidth

    childrenWidth :: Int
    childrenWidth =
      case (nub $ (runTree >>> _.width) <$> ts) of
        Cons tsw Nil -> tsw
        _ -> one
      
  
    t :: TreeRec 
    t = runTree $ foldl mergeTrees (Tree $ tree {height = zero})
        (ts >>= runTree >>> _.children)

    ts :: List Tree
    ts = toList (treeFromJson maxTupleSize label path <$> ja)


  obj :: JObject -> Tree
  obj jo =
    if M.isEmpty jo
    then Tree tree
    else Tree $ tree {width = width, height = height, children = children }
    where
    assocToTree :: JAssoc -> Tree
    assocToTree (Tuple l j) =
      treeFromJson maxTupleSize l (snoc path l) j
      
    children :: List Tree
    children = assocToTree <$> M.toList jo

    width :: Int
    width = foldl (+) zero $ (runTree >>> _.width) <$> children

    height :: Int
    height = one + (foldl (+) zero $ (runTree >>> _.height) <$> children)

widthOfPrimTuple :: Int -> JPath -> JArray -> Maybe Int
widthOfPrimTuple maxTupleSize path ja = do
  head path
  ja A.!! 1
  for ja toPrim
  let l = A.length ja 
  guard $ l <= maxTupleSize
  pure l


mergeTrees :: Tree -> Tree -> Tree
mergeTrees (Tree t) (Tree nt) =
  maybe notChild go (i >>= \ix -> t.children !! ix)
  where
  i :: Maybe Int
  i = findIndex (runTree >>> _.label >>> \x -> x == nt.label) t.children

  notChild :: Tree
  notChild = Tree $ t { width = width, height = height, children = children}
    where
    width :: Int
    width = if null t.children
            then nt.width
            else t.width + nt.width

    height :: Int
    height = max t.height (nt.height + one)

    children :: List Tree
    children = snoc t.children (Tree nt)

  go :: Tree -> Tree
  go c@(Tree child) =
    Tree child { width = width, height = height, children = children }
    where
    width :: Int
    width = t.width - child.width + maxDescendantWidth

    height :: Int
    height = max t.height (subChild.height + one)

    children :: List Tree
    children = 
      fromMaybe t.children $ do
        ix <- i 
        updateAt ix (Tree child { width = maxDescendantWidth
                               , height = subChild.height
                               , children = subChild.children
                               }) t.children

    subChild :: TreeRec 
    subChild = runTree $ foldl mergeTrees c nt.children

    maxDescendantWidth :: Int
    maxDescendantWidth =
      max subChild.width if null nt.children && not (null subChild.children)
                         then one
                         else nt.width


mkTable :: Int -> Tree -> JCursor -> Json -> Table
mkTable maxTupleSize (Tree t) c =
  foldJsonPrim prim array obj
  where
  width :: JArray -> Maybe Int
  width ja =
    if null t.children && t.width > 1
    then widthOfPrimTuple maxTupleSize t.path ja
    else Nothing

  prim :: JsonPrim -> Table
  prim jp = singleton $ singleton $ Cell { cursor: c
                                         , width: t.width
                                         , height: one
                                         , json: jp
                                         }

  array :: JArray -> Table
  array ja = fromMaybe (jarr ja) $ tuple ja

  tuple :: JArray -> Maybe Table
  tuple ja = (primtup ja) <|> (objtup ja)

  primtup :: JArray -> Maybe Table
  primtup ja = do
    width ja
    pure $ singleton $ mkCells <$> (range zero (t.width - one))
    where
    mkCells :: Int -> Cell
    mkCells i =
      Cell { cursor: downIndex i c
           , width: one
           , height: one
           , json: fromMaybe primNull (ja A.!! i >>= toPrim)
           } 

  objtup :: JArray -> Maybe Table
  objtup ja = mergeObjTuple maxTupleSize (Tree t) c ja

  jarr :: JArray -> Table
  jarr ja =
    (enumerate $ toList ja) >>= \(Tuple j i) ->
    mkTable maxTupleSize (Tree t) (downIndex i c) j
  
  obj :: JObject -> Table
  obj jo =
    if M.isEmpty jo
    then singleton $ singleton $ Cell { cursor: c
                                      , width: t.width
                                      , height: one
                                      , json: primNull
                                      }
    else mergeTableTuples $ labeledTable <$> t.children
    where
    labeledTable :: Tree -> Tuple Int Table
    labeledTable (Tree tree) =
      let j = fromMaybe (primToJson primNull) $ M.lookup tree.label jo in
      Tuple tree.width (mkTable maxTupleSize (Tree tree) (downField tree.label c) j)


mergeObjTuple :: Int -> Tree -> JCursor -> JArray -> Maybe Table
mergeObjTuple maxTupleSize (Tree t) c ja = do
  A.head ja
  jos <- for ja toObject
  let kss :: Array (Array String)
      kss = M.keys <$> jos
      ks :: Array String
      ks = A.concat kss
  guard $ A.length ks == (A.length $ A.nub ks)
  let mkTableTuple :: Tree -> Maybe (Tuple Int Table)
      mkTableTuple (Tree tree) = do
        i <- A.findIndex (elem tree.label) kss
        obj <- jos A.!! i
        let j = fromMaybe (primToJson primNull) $ M.lookup tree.label obj
        pure $ Tuple tree.width
          (mkTable maxTupleSize (Tree tree) (downField tree.label $ downIndex i c) j)
          
  pure $ mergeTableTuples $ catMaybes (mkTableTuple <$> t.children)


mergeTableTuples :: List (Tuple Int Table) -> Table
mergeTableTuples tables =
  oneColumn <$> (range zero $ max zero $ maxh - one)
  where
  maxh :: Int
  maxh = foldl max 0 $ (length <<< snd) <$> tables

  oneColumn :: Int -> List Cell
  oneColumn n =
    tables >>= \(Tuple width table) ->
    let rnOr = flip fromMaybe (table !! n)
    in case table of
      Cons r Nil ->
        if n == 0
        then (\(Cell c) -> Cell $ c{height = maxh}) <$> r
        else rnOr Nil

      _ -> rnOr $ singleton $ Cell { cursor: JCursorTop
                                   , width: width
                                   , height: one
                                   , json: primNull
                                   }

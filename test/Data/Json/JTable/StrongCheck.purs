module Test.Data.Json.JTable.StrongCheck where

import Data.Array (null, length, head, last, snoc, nub, (..))
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Data.Argonaut.Core (Json(..), jsonNull)
import Data.Argonaut.JCursor (JCursor(..))
import qualified Text.Smolder.HTML as H
import Text.Smolder.Markup (Markup(..), text)
import qualified Text.Smolder.Renderer.String (render) as Sm
import Test.StrongCheck (quickCheck, Result(..), (<?>))
import Debug.Trace (print, trace)
import Debug.Spy (spy)

import Data.Json.Gen (genJson)
import Data.Json.JTable.Internal (
  Tree(..), tWidth, tHeight, tKids, tFromJson, tsToRows,
  Cell(..), cWidth, cHeight, cFromJson, renderRows)


-- repeat cells with [row|col]span 
unprojectT :: forall a. (a -> Number) -> (a -> Number) -> [[a]] -> [[a]]
unprojectT widthFn heightFn rows = 
  fromMaybe [] $ head rows <#> \_-> let
    rows' = rows <#> (>>= \cell -> (0..(widthFn cell - 1)) <#> \_->
      Tuple cell (heightFn cell))
    op = \rs nrow -> snoc rs $ fromMaybe nrow $ last rs <#> \r -> let
      op' = \(Tuple done rest) (Tuple cell h) ->
        if h > 1 
        then Tuple (snoc done (Tuple cell (h-1))) rest
        else case rest of (c : cs) -> Tuple (snoc done c) cs
                          _        -> Tuple (snoc done $ Tuple cell 0) []
      in fst $ foldl op' (Tuple [] nrow) r 
    in (foldl op [] rows') <#> (<#> fst)

unprojectTreeT = unprojectT tWidth tHeight
unprojectCellT = unprojectT cWidth cHeight

-- check that all rows are the same length
isRectangularT :: forall a. [[a]] -> Boolean
isRectangularT t = (length $ nub $ t <#> length) <= 1

-- check that thead and tbody are the same width (or 0)
lengthsOkT :: forall a b. [[a]] -> [[b]] -> Boolean
lengthsOkT t1 t2 = 
  fromMaybe (null t1 || null t2) $ 
    head t1 >>= \ht1 -> head t2 <#> \ht2 ->
      (null ht1) || (null ht2) || (length ht1) == (length ht2)

-- check table invariants
checkTable :: Json -> Result
checkTable json = let
  t = tFromJson 2 "" [] json
  hrs = tsToRows (t # tKids)
  drs = cFromJson 2 t JCursorTop json
  uhrs = unprojectTreeT $ hrs
  udrs = unprojectCellT $ drs
  in (isRectangularT uhrs) && (isRectangularT udrs) && 
     (lengthsOkT uhrs udrs) <?> show json 


-- print unprojected html for debugging
barf :: Json -> String
barf json = let 
  t = tFromJson 2 "" [] json
  hrs = tsToRows (t # tKids)
  drs = cFromJson 2 t JCursorTop json
  th' = \y x (T l p w h k) -> H.th $ text $ show l
  td' = \y x   (C c w h j) -> H.td $ text $ show j
  in Sm.render $ H.table $ do 
    H.thead $ renderRows H.tr th' $ unprojectTreeT hrs
    H.tbody $ renderRows H.tr td' $ unprojectCellT drs

-- foreign import j """var j = null""" :: Json

main = do
  quickCheck checkTable
  -- print $ checkTable j
  -- trace $ barf j

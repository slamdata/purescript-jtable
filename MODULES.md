# Module Documentation

## Module Data.Json.JTable

### Types


    type ColumnOrdering = JPath -> JPath -> Ordering


    type JTableOpts = { maxHomoTupSize :: Number, insertHeaderCells :: Boolean, columnOrdering :: ColumnOrdering, style :: TableStyle }

     type JPath = [String]

    type TableStyle = { td :: JCursor -> JsonPrim -> Markup, th :: JPath -> Markup, tr :: Markup -> Markup, table :: Markup -> Markup }


### Values


    renderJTable :: JTableOpts -> Json -> Markup


    renderJTableArray :: JTableOpts -> [Json] -> Markup


    renderJTableDef :: Json -> Markup


## Module Data.Json.JTable.Internal

### Types

     cell data

    data Cell where
      C :: JCursor -> Number -> Number -> JsonPrim -> Cell

     path of object keys, with array indices omitted

    type JPath = [String]

     rows of cells

    type Table = [[Cell]]

     header data

    data Tree where
      T :: JPath -> Number -> Number -> [Tree] -> Tree


### Type Class Instances


    instance showCell :: Show Cell


    instance showTree :: Show Tree


### Values


    _nattr :: String -> Number -> Markup -> Markup

     produce data table from json, according to header tree

    cFromJson :: Number -> Tree -> JCursor -> Json -> Table

     merge table segments for each key of an object into one

    cMergeObj :: [Tuple Number Table] -> Table


    foldJsonP :: forall a. (JsonPrim -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a

     pad tall header cells from above

    insertHeaderCells :: Number -> Tree -> Tree

     maybe merge a tuple of objects into a table segment

    mergeObjTuple :: Number -> Tree -> JCursor -> [Json] -> Maybe Table

     render a grid from an array of arrays

    renderRows :: forall a. (Markup -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> Markup


    renderTbody :: (Markup -> Markup) -> (Cell -> Markup) -> Tree -> Table -> Markup


    renderThead :: (Markup -> Markup) -> (Tree -> Markup) -> Tree -> Markup

     sort header tree by ColumnOrdering

    sortTree :: (JPath -> JPath -> Ordering) -> Tree -> Tree


    strcmp :: String -> String -> Ordering

     produce a tree of header data from json

    tFromJson :: Number -> JPath -> Json -> Tree

     add child to tree, unify if exists

    tMergeArray :: Tree -> Tree -> Tree


    toPrim :: Json -> Maybe JsonPrim

     produce header rows from header tree

    tsToRows :: [Tree] -> [[Tree]]

     maybe return the width of a tuple composed of primitive values

    widthOfPrimTuple :: Number -> JPath -> [Json] -> Maybe Number


## Module Data.Json.JSemantic

### Types


    data JSemantic where
      Integral :: Number -> JSemantic
      Fractional :: Number -> JSemantic
      Date :: Date.Date -> JSemantic
      DateTime :: Date.Date -> JSemantic
      Time :: Date.Date -> JSemantic
      Interval :: Date.Date -> Date.Date -> JSemantic
      Text :: String -> JSemantic
      Bool :: Boolean -> JSemantic
      Percent :: Number -> JSemantic
      Currency :: Number -> JSemantic
      NA :: JSemantic


### Type Class Instances


    instance eqJSemantic :: Eq JSemantic


    instance showJSemantic :: Show JSemantic


### Values

     TODO: date, time

    toSemantic :: JsonPrim -> JSemantic




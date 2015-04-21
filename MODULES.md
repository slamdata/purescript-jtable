# Module Documentation

## Module Data.Json.JTable

#### `noStyle`

``` purescript
noStyle :: TableStyle
```


#### `bootstrapStyle`

``` purescript
bootstrapStyle :: TableStyle
```


#### `debugStyle`

``` purescript
debugStyle :: TableStyle
```


#### `inOrdering`

``` purescript
inOrdering :: ColumnOrdering
```


#### `alphaOrdering`

``` purescript
alphaOrdering :: ColumnOrdering
```


#### `jTableOptsDefault`

``` purescript
jTableOptsDefault :: JTableOpts
```


#### `renderJTable`

``` purescript
renderJTable :: JTableOpts -> Json -> Markup
```


#### `renderJTableDef`

``` purescript
renderJTableDef :: Json -> Markup
```



## Module Data.Json.JTable.Internal

#### `JPath`

``` purescript
type JPath = [String]
```

#### `Table`

``` purescript
type Table = [[Cell]]
```

#### `Tree`

``` purescript
newtype Tree
  = Tree { kids :: [Tree], height :: Number, width :: Number, path :: JPath, label :: String }
```

#### `Cell`

``` purescript
newtype Cell
  = Cell { json :: JsonPrim, height :: Number, width :: Number, cursor :: JCursor }
```

#### `Markup`

``` purescript
type Markup = H.HTML Void Void
```


#### `TableStyle`

``` purescript
type TableStyle = { td :: JCursor -> JsonPrim -> Number -> Number -> Markup, th :: String -> JPath -> Number -> Number -> Markup, tr :: [Markup] -> Markup, table :: [Markup] -> Markup }
```


#### `ColumnOrdering`

``` purescript
type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering
```


#### `JTableOpts`

``` purescript
type JTableOpts = { maxTupleSize :: Number, insertHeaderCells :: Boolean, columnOrdering :: ColumnOrdering, style :: TableStyle }
```


#### `foldJsonP`

``` purescript
foldJsonP :: forall a. (JsonPrim -> a) -> (JArray -> a) -> (JObject -> a) -> Json -> a
```


#### `toPrim`

``` purescript
toPrim :: Json -> Maybe JsonPrim
```


#### `zipWithIndex`

``` purescript
zipWithIndex :: forall a. [a] -> [Tuple a Number]
```


#### `widthOfPrimTuple`

``` purescript
widthOfPrimTuple :: Number -> JPath -> [Json] -> Maybe Number
```

Maybe return the width of a tuple composed of primitive values

#### `tMergeArray`

``` purescript
tMergeArray :: Tree -> Tree -> Tree
```

Add child to tree, unify if exists

#### `tFromJson`

``` purescript
tFromJson :: Number -> String -> JPath -> Json -> Tree
```

Produce a tree of header data from json

#### `cMergeObj`

``` purescript
cMergeObj :: [Tuple Number Table] -> Table
```

Merge table segments for each key of an object into one

#### `mergeObjTuple`

``` purescript
mergeObjTuple :: Number -> Tree -> JCursor -> [Json] -> Maybe Table
```

Maybe merge a tuple of objects into a table segment

#### `cFromJson`

``` purescript
cFromJson :: Number -> Tree -> JCursor -> Json -> Table
```

Produce data table from json, according to header tree

#### `renderRows`

``` purescript
renderRows :: forall a. ([Markup] -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> [Markup]
```

Render a grid from an array of arrays

#### `tsToRows`

``` purescript
tsToRows :: [Tree] -> [[Tree]]
```

Produce header rows from header tree

#### `renderThead`

``` purescript
renderThead :: ([Markup] -> Markup) -> (String -> JPath -> Number -> Number -> Markup) -> Tree -> Markup
```


#### `renderTbody`

``` purescript
renderTbody :: ([Markup] -> Markup) -> (JCursor -> JsonPrim -> Number -> Number -> Markup) -> Tree -> Table -> Markup
```


#### `sortTree`

``` purescript
sortTree :: ColumnOrdering -> Tree -> Tree
```

Sort header tree by ColumnOrdering

#### `strcmp`

``` purescript
strcmp :: String -> String -> Ordering
```


#### `padTree`

``` purescript
padTree :: Number -> Tree -> Tree
```

Pad tall header cells from above

#### `renderJTableRaw`

``` purescript
renderJTableRaw :: JTableOpts -> Json -> Markup
```



## Module Data.Json.JSemantic

#### `JSemantic`

``` purescript
data JSemantic
  = Integral Number
  | Fractional Number
  | Date Date.Date
  | DateTime Date.Date
  | Time Date.Date
  | Interval Date.Date Date.Date
  | Text String
  | Bool Boolean
  | Percent Number
  | Currency Number
  | NA 
```


#### `showJSemantic`

``` purescript
instance showJSemantic :: Show JSemantic
```


#### `eqJSemantic`

``` purescript
instance eqJSemantic :: Eq JSemantic
```


#### `JSemanticOpts`

``` purescript
type JSemanticOpts = { regexps :: { date :: Regex, currency :: Regex, percent :: Regex } }
```


#### `toSemantic`

``` purescript
toSemantic :: JSemanticOpts -> JsonPrim -> JSemantic
```

#### `toSemanticDef`

``` purescript
toSemanticDef :: JsonPrim -> JSemantic
```



## Module Data.Json.Gen

#### `genJsonArray`

``` purescript
genJsonArray :: Gen Json
```


#### `genJsonObject`

``` purescript
genJsonObject :: Gen Json
```


#### `genJson`

``` purescript
genJson :: Gen Json
```


#### `arbJson`

``` purescript
instance arbJson :: Arbitrary Json
```





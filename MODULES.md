# Module Documentation

## Module Data.Json.JTable

#### `TableStyle`

``` purescript
type TableStyle = { td :: JCursor -> JsonPrim -> Markup, th :: String -> JPath -> Markup, tr :: Markup -> Markup, table :: Markup -> Markup }
```

#### `ColumnOrdering`

``` purescript
type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering
```


#### `JTableOpts`

``` purescript
type JTableOpts = { maxHomoTupSize :: Number, insertHeaderCells :: Boolean, columnOrdering :: ColumnOrdering, style :: TableStyle }
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
data Tree
  = T String JPath Number Number [Tree]
```

#### `showTree`

``` purescript
instance showTree :: Show Tree
```


#### `Cell`

``` purescript
data Cell
  = C JCursor Number Number JsonPrim
```

#### `showCell`

``` purescript
instance showCell :: Show Cell
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


#### `orelse`

``` purescript
orelse :: forall a b. (a -> Maybe b) -> (a -> b) -> a -> b
```


#### `widthOfPrimTuple`

``` purescript
widthOfPrimTuple :: Number -> JPath -> [Json] -> Maybe Number
```

#### `tMergeArray`

``` purescript
tMergeArray :: Tree -> Tree -> Tree
```

#### `tFromJson`

``` purescript
tFromJson :: Number -> String -> JPath -> Json -> Tree
```

#### `cMergeObj`

``` purescript
cMergeObj :: [Tuple Number Table] -> Table
```

#### `mergeObjTuple`

``` purescript
mergeObjTuple :: Number -> Tree -> JCursor -> [Json] -> Maybe Table
```

#### `cFromJson`

``` purescript
cFromJson :: Number -> Tree -> JCursor -> Json -> Table
```

#### `renderRows`

``` purescript
renderRows :: forall a. (Markup -> Markup) -> (Number -> Number -> a -> Markup) -> [[a]] -> Markup
```

#### `_nattr`

``` purescript
_nattr :: String -> Number -> Markup -> Markup
```


#### `tsToRows`

``` purescript
tsToRows :: [Tree] -> [[Tree]]
```

#### `renderThead`

``` purescript
renderThead :: (Markup -> Markup) -> (Tree -> Markup) -> Tree -> Markup
```


#### `renderTbody`

``` purescript
renderTbody :: (Markup -> Markup) -> (Cell -> Markup) -> Tree -> Table -> Markup
```


#### `sortTree`

``` purescript
sortTree :: (String -> JPath -> String -> JPath -> Ordering) -> Tree -> Tree
```

#### `strcmp`

``` purescript
strcmp :: String -> String -> Ordering
```


#### `padTree`

``` purescript
padTree :: Number -> Tree -> Tree
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





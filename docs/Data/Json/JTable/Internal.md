## Module Data.Json.JTable.Internal

#### `JPath`

``` purescript
type JPath = List String
```

#### `JTableQuery`

``` purescript
data JTableQuery a
  = SetJson Json a
```

#### `Markup`

``` purescript
type Markup = HTML Void (JTableQuery Unit)
```

#### `TableStyle`

``` purescript
type TableStyle = { table :: Array Markup -> Markup, tr :: Array Markup -> Markup, th :: String -> JPath -> Int -> Int -> Markup, td :: JCursor -> JsonPrim -> Int -> Int -> Markup }
```

#### `ColumnOrdering`

``` purescript
type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering
```

#### `JTableOpts`

``` purescript
type JTableOpts = { style :: TableStyle, columnOrdering :: ColumnOrdering, insertHeaderCells :: Boolean, maxTupleSize :: Int }
```

#### `renderJTableRaw`

``` purescript
renderJTableRaw :: JTableOpts -> Json -> Markup
```



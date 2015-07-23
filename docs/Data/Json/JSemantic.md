## Module Data.Json.JSemantic

#### `TimeRec`

``` purescript
type TimeRec = { hours :: Hours, minutes :: Minutes, seconds :: Seconds, milliseconds :: Milliseconds }
```

#### `JSemantic`

``` purescript
data JSemantic
  = Integral Int
  | Fractional Number
  | Date Date
  | DateTime Date
  | Time TimeRec
  | Interval Date Date
  | Text String
  | Bool Boolean
  | Percent Number
  | Currency String Number
  | NA
```

##### Instances
``` purescript
instance showJSemantic :: Show JSemantic
instance eqJSemantic :: Eq JSemantic
```

#### `renderJSemantic`

``` purescript
renderJSemantic :: JSemantic -> String
```

#### `JSemanticParsers`

``` purescript
type JSemanticParsers = { boolParsers :: List (Boolean -> Maybe JSemantic), numberParsers :: List (Number -> Maybe JSemantic), stringParsers :: List (String -> Maybe JSemantic) }
```

#### `defaultParsers`

``` purescript
defaultParsers :: JSemanticParsers
```

#### `toSemantic`

``` purescript
toSemantic :: JSemanticParsers -> JsonPrim -> JSemantic
```

#### `toSemanticDef`

``` purescript
toSemanticDef :: JsonPrim -> JSemantic
```



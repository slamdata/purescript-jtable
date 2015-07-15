## Module Data.Json.JSemantic

#### `JSemantic`

``` purescript
data JSemantic
  = Integral Int
  | Fractional Number
  | Date Date
  | DateTime Date
  | Time Date
  | Interval Date Date
  | Text String
  | Bool Boolean
  | Percent Number
  | Currency Number
  | NA
```

##### Instances
``` purescript
instance showJSemantic :: Show JSemantic
instance eqJSemantic :: Eq JSemantic
```

#### `JSemanticRegexes`

``` purescript
type JSemanticRegexes = { percent :: Regex, currency :: Regex, date :: Regex }
```

#### `JSemanticOpts`

``` purescript
type JSemanticOpts = { regexps :: JSemanticRegexes }
```

#### `jSemanticOptsDefault`

``` purescript
jSemanticOptsDefault :: JSemanticOpts
```

#### `toSemantic`

``` purescript
toSemantic :: JSemanticOpts -> JsonPrim -> JSemantic
```

#### `toSemanticDef`

``` purescript
toSemanticDef :: JsonPrim -> JSemantic
```



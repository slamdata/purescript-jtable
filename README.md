# purescript-jtable

A small but powerful Purescript library to render arbitrary JSON into HTML tables. 

Turn this:

```json
{
  "userId": 8927524,
  "profile": {
    "name":   "Mary Jane",
    "age":    29,
    "gender": "female"
  },
  "comments": [{
    "id":       "F2372BAC",
    "text":     "I concur.",
    "replyTo":  [9817361, "F8ACD164F"],
    "time":     "2015-02-03"
  }, {
    "id":       "GH732AFC",
    "replyTo":  [9654726, "A44124F"],
    "time":     "2015-03-01"
  }]
}
```

Into this:

```
|----------|----------------------------|------------------------------------------------------------|
|          |           profile          |                           comments                         |
|----------|-----------|-----|----------|----------|--------------|---------------------|------------|
|  userId  |    name   | age |  gender  |    id    |     text     |       replyTo       |    time    |
|----------|-----------|-----|----------|----------|--------------|---------------------|------------|
|  8927524 | Mary Jane |  29 |  female  | F2372BAC | I concur.    | 9817361 | F8ACD164F | 2015-02-03 |
|          |           |     |          |----------|--------------|---------------------|------------|
|          |           |     |          | GH732AFC |              | 9654726 | A44124F   | 2015-03-01 |
|----------|-----------|-----|----------|------------------------------------------------------------|
```

[Try it!](http://rawgit.com/brainrape/purescript-jtable/dev-squash/examples/try.html) with your own data or some samples.


## How?

Install with bower: `$ bower install slamdata/purescript-jtable`
Or use the compiled browser version at [examples/jtable.js](examples/jtable.js)


### Example Usage

```purescript
-- default options
renderJTableDef json

-- alphabetic column ordering, show full path with length
renderJTable jTableOptsDefault {
  style = noStyle { th = (\path -> th $ text $ 
    (show $ length path) ++ " " ++ show path) },
  columnOrdering = alphaOrdering } json

```
See the [Try it!](http://rawgit.com/brainrape/purescript-jtable/dev-squash/examples/try.html) page and [examples/Examples.purs](examples/Examples.purs) for more usage examples.


### API

```purescript
-- rendering functions
renderJTable      :: JTableOpts -> Json -> Markup    -- main renderer
renderJTableDef   ::               Json -> Markup    -- use default options

type JTableOpts = {                 -- rendering options
  style :: TableStyle,              -- override element rendering
  columnOrdering :: ColumnOrdering, -- customize header ordering
  insertHeaderCells :: Boolean,     -- pad tall headers with empty cells above
  maxTupleSize :: Number}           -- arrays of primitives upto this size 
                                       -- are rendered horizontally as tuples
jTableOptsDefault :: JTableOpts     -- default options for easy overriding

type JPath = [String]  -- object key hierarchy with array indices omitted

-- override element rendering. make sure to produce the right elements
type TableStyle = {  
  table :: Markup -> Markup,
  tr    :: Markup -> Markup,
  th    :: String -> JPath -> Markup,  -- params: label (object key) and path
  td    :: JCursor -> JsonPrim -> Markup }

-- example styles to get started
noStyle        :: TableStyle  -- plain rendering
debugStyle     :: TableStyle  -- shows paths for each cell
bootstrapStyle :: TableStyle  -- adds "table" class to table

type ColumnOrdering = JPath -> JPath -> Ordering 
inOrdering    :: ColumnOrdering  -- example ordering: noop
alphaOrdering :: ColumnOrdering  -- example ordering: alphabetic
```

The `Json` and `JCursor` data types that appear in the API are from [purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut). The `Markup` data type that appears in the API is from [purescript-smolder](https://github.com/bodil/purescript-smolder). 

Both of these libraries are listed as dependencies in [bower.json](bower.json).

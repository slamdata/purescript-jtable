# purescript-jtable

A small but powerful Purescript library to render heterogeneous arrays of JSON into HTML nodes that display multi-dimensional tables. The rendering degrades gracefully for flat data (to ordinary tables).

**Note**: *SlamData is offering a $2K bounty to whoever authors the first Pull Request that we merge into this repository. This is a great way to learn some Purescript, as well as contribute to a 100% open source library released under a commercial-friendly MIT license.*

If you decide to take on this project, there is [some code you may extract from SlamData](https://github.com/slamdata/slamdata/blob/master/src/SlamData/Data/Analyze.purs), which should be generalized and submitted to [purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut).

## API

The `Json` and `JPath` data types that appear in the API are from [purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut). The `Markup` data type that appears in the API is from [purescript-smolder](https://github.com/bodil/purescript-smolder). 

Both of these libraries are listed as dependencies in [bower.json](bower.json).

```purescript

-- Data.Argonaut.JsonSemantic

data JSemantic =       -- this could be moved to Data.Argonaut
    Integral    |
    Fractional  |
    Date        |      -- ISO8601 & family
    DateTime    |
    Time        |
    Interval    |
    Text        |
    Bool        |
    Percent     |
    Currency    |
    NA

-- Data.Json.Render

type Level = Number

data ColumnOrdering = InOrdering | CustomOrdering (JPath -> JPath -> Ordering)

data TableStyle = 
  TableStyle { 
    table   :: Markup -> Markup,
    cell    :: JSemantic -> Markup -> Markup, 
    head    :: Markup -> Markup,
    row     :: Markup -> Markup }

renderJTable :: TableStyle -> ColumnOrdering -> [Json] -> Markup

-- Data.Json.Render.Styles

-- | This style doesn't require bootstrap, only made to look like it
bootstrapStyle :: TableStyle 

-- | This style doesn't require foundation, only made to look like it
foundationStyle :: TableStyle
```

## Algorithm

1. Classify every leaf node using `JSemantic` (leaf nodes include `null`, `String`, `Boolean`, and `Number`).
2. Classify every array as either a tuple or a list, according to heterogeneity.
   * Heterogeneous arrays of the same length are classified as tuples.
   * Homogeneous arrays of differing lengths are classified as lists.
   * Consider two object schemas homogeneous if one has a strict superset of the fields of the other one (i.e. ignore differences due to non-existence).
3. Push arrays to the leaf nodes.
4. Pull apart the JSON objects into a list of tuples consisting of a JSON path (e.g. `.profile.name`), and either a leaf node, or an array of leaf nodes.
5. Determine the number of levels in the table headers by finding the deepest JSON path in the list of tuples (e.g. `.profile.name` has a depth of 2).
6. Treat the unique set of JSON paths in the list of tuples as the (hierarchical) columns for the table.
7. Render the columns hierarchically.
8. Render the content in straightforward fashion. Arrays of leaves are rendered differently depending on their type:
   * **Tuples**. Tuples are rendered as inline, divided, anonymous cells.
   * **Lists**. Lists are rendered vertically.


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

|          |           profile          |                           comments                         |
|----------|-----------|-----|----------|----------|--------------|---------------------|------------|
|  userId  |    name   | age |  gender  |    id    |     text     |       replyTo       |    time    |
|----------|-----------|-----|----------|----------|--------------|---------------------|------------|
|  8927524 | Mary Jane |  29 |  female  | F2372BAC | I concur.    | 9817361 | F8ACD164F | 2015-02-03 |
|          |           |     |          |----------|--------------|---------------------|------------|
|          |           |     |          | GH732AFC |              | 9654726 | A44124F   | 2015-03-01 |
|          |           |     |          |----------|--------------|---------------------|------------|
|          |           |     |          |                                                        ... |


## Examples

The [examples](/examples) directory contains a handful of example tables that are rendered with this library, as well as an editable text form that allows you to copy / paste JSON into the form and click *Render* to see how the library renders the JSON data.

## Tests

The library contains an extensive suite of unit tests written using [purescript-strongcheck](https://github.com/purescript-contrib/purescript-strongcheck), which verify correct translation of nested, heterogeneous, array-filled JSON data into clean tabular markup.

# purescript-jtable

A small but powerful Purescript library to render heterogeneous arrays of JSON into HTML nodes that display multi-dimensional tables. The rendering degrades gracefully for flat data (to ordinary tables).

**Note**: *SlamData is offering a $2K bounty to whoever authors the first Pull Request merged into this repository that satisfies the requirements of the project. This is a great way to learn some Purescript, as well as contribute to a 100% open source library released under a commercial-friendly MIT license.*

If you decide to take on this project, there is [some code you may extract from SlamData](https://github.com/slamdata/slamdata/blob/master/src/SlamData/Data/Analyze.purs), which should be generalized and submitted to [purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut).

## API

The `Json` and `JCursor` data types that appear in the API are from [purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut). The `Markup` data type that appears in the API is from [purescript-smolder](https://github.com/bodil/purescript-smolder). 

Both of these libraries are listed as dependencies in [bower.json](bower.json).

```purescript

-- Data.Argonaut.JSemantic

data JSemantic =       -- this could be moved to Data.Argonaut
    Integral    |
    Fractional  |
    Date        |
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

data ColumnOrdering = InOrdering | CustomOrdering (JCursor -> JCursor -> Ordering)

-- Table style does not create the structure of a table, it only styles
-- a table that's already been created.
data TableStyle = 
  TableStyle { 
    table   :: Level      -> -- is this a top-level table? or a nested table?
               Markup     -> -- the unstyled <table> element
               Markup,       -- the styled <table> element

    td      :: JSemantic  -> -- what type of cell (for purposes of formatting only!)
               Markup     -> -- the unstyled <td> element
               Markup,       -- the styled <td> element

    th      :: JCursor    -> -- the path associated with the header
               Markup     -> -- the unstyled <th> element
               Markup,       -- the styled <th> element

    tr      :: Markup     -> -- the unstyled <tr> element
               Markup }      -- the styled <tr> element

renderJTable :: TableStyle      -> -- the style to apply to the table
                ColumnOrdering  -> -- how to order the columns
                [Json]          -> -- the array of JSON values
                Markup             -- the final HTML markup for the table

-- Data.Json.Render.Styles

-- | This style doesn't require bootstrap, only made to look like it
bootstrapStyle :: TableStyle 

-- | This style doesn't require foundation, only made to look like it
foundationStyle :: TableStyle
```

## Algorithm

**Note:** *The algorithm does not have to be implemented in this fashion, but the end result should be the same.*

1. Classify every leaf node using `JSemantic` (leaf nodes include `null`, `String`, `Boolean`, and `Number`), based on the most common type of semantic detected in the data (this is used only for formatting).
2. Classify every array as either a tuple or a list, according to heterogeneity.
   * Heterogeneous arrays of the same length are classified as tuples.
   * Homogeneous arrays of differing lengths are classified as lists.
   * When comparing object schemas, ignore differences due to nulls or non-existence. For example, `{"foo": 1}` should be considered to have the same schema as `{"foo": 1, "bar": 2}`, and `{"foo": 1, "baz": null}`.
3. Push arrays to the leaf nodes.
4. Pull apart the JSON objects into a list of tuples consisting of a JSON path (e.g. `.profile.name`), and either a leaf node, or an array of leaf nodes (`[Tuple JCursor (Either [JsonPrimitive] JsonPrimitive)]`).
5. Determine the number of levels in the table headers by finding the deepest JSON path in the list of tuples (e.g. `.profile.name` has a depth of 2).
6. Treat the unique set of JSON paths in the list of tuples as the (hierarchical) columns for the table.
7. Render the columns hierarchically.
8. Render the content in straightforward fashion (do not render `null` or undefined values, leave those cells blank!). Arrays of leaves are rendered differently depending on their type:
   * **Tuples**. Tuples are rendered as inline, divided, anonymous cells.
   * **Lists**. Lists are rendered vertically.

### Worked Example

Assume the following JSON:


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

1. Assuming the above JSON sample, we first classify the leaf nodes and arrays:
   * `.userId` -> `Integral`
   * `.profile.name` -> `Text`
   * `.profile.age` -> `Integral`
   * `.profile.gender` -> `Text`
   * `.profile.comments[*]` -> Array
   * `.profile.comments[*].id` -> `Text`
   * `.profile.comments[*].text` -> `Text`
   * `.profile.comments[*].replyTo[*]` -> Tuple
   * `.profile.comments[*].replyTo[0]` -> `Integral`
   * `.profile.comments[*].replyTo[1]` -> `Text`
   * `.profile.comments[*].time` -> `Date`
2. Now we push all arrays to leaf nodes, producing the following structure:

   ```json
    {
      "userId": 8927524,
      "profile": {
        "name":   "Mary Jane",
        "age":    29,
        "gender": "female"
      },
      "comments": {
        "id":       ["F2372BAC", "GH732AFC"],
        "text":     ["I concur.", null],
        "replyTo":  [[9817361, "F8ACD164F"], [9654726, "A44124F"]],
        "time":     ["2015-02-03", "2015-03-01"]
      }
    }
   ```
3. Now we split apart the JSON paths from the leaf nodes (represented below by JSON, since it's easier to read):

   ```json
    [
      [".userId",           8927524],
      [".profile.name",     "Mary Jane"],
      [".profile.age",      29],
      [".profile.gender",   "female"],
      [".comments.id",      ["F2372BAC", "GH732AFC"]],
      [".comments.text",    ["I concur.", null]],
      [".comments.replyTo", [[9817361, "F8ACD164F"], [9654726, "A44124F"]]],
      [".comments.time",    ["2015-02-03", "2015-03-01"]]
    ]
   ```
4. We determine the depth of the table headings to be 2, because that is the maximum depth of any `JCursor` that appears in the list of tuples.
5. We now render the tuples as a hierarchical table, as specified above.

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

Note the difference in how `comments[*]` is rendered versus `replyTo[*]`, because the first one is classified as a list, while the second is classified as a tuple.

### Tuple Examples

```json
[
{
 "voter": "Smith",
 "preferences": [{"id": 123891, "label": "How to draw"}, {"id": 981234, "label": "How to climb stairs"}, {"id": 89231, "label": "How to paint"}]
},
{
 "voter": "Mary",
 "preferences": [{"id": 981234, "label": "How to climb stairs"}, {"id": 89231, "label": "How to paint"}, {"id": 123891, "label": "How to draw"}]
},
{
 "voter": "Allen",
 "preferences": [{"id": 89231, "label": "How to paint"}, {"id": 123891, "label": "How to draw"}, {"id": 981234, "label": "How to climb stairs"}]
}
]
```

```
|-------|------------------------------|
|       |         preferences          |
|-------|--------|---------------------|
| voter |   id   |        label        |
|-------|--------|---------------------|
| Smith | 123891 | How to draw         |
|       |--------|---------------------|
|       | 981234 | How to climb stairs |
|       |--------|---------------------|
|       |  89231 | How to paint        |
|-------|--------|---------------------|
| Mary  | 981234 | How to climb stairs |
|       |--------|---------------------|
|       |  89231 | How to paint        |
|       |--------|---------------------|
|       | 123891 | How to draw         |
|-------|--------|---------------------|
| Allen |  89231 | How to paint        |
|       |--------|---------------------|
|       | 123891 | How to draw         |
|       |--------|---------------------|
|       | 981234 | How to climb stairs |
|-------|--------|---------------------|
```

---------------------

```json
[
{
 "voter": "Smith",
 "ranking": [0, 2, 3, 1, 8]
},
{
 "voter": "Mary",
 "ranking": [1, 8, 3, 0, 2]
},
{
 "voter": "Allen",
 "ranking": [3, 0, 8, 1, 2]
}
]
```

```
|-------|-------------------|
| voter |      ranking      |
|-------|-------------------|
| Smith | 0 | 2 | 3 | 1 | 8 |
|-------|-------------------|
| Mary  | 1 | 8 | 3 | 0 | 2 |
|-------|-------------------|
| Allen | 3 | 0 | 8 | 1 | 2 |
|-------|-------------------|
```

**Note**: This example will always be rendered like this regardless of the size of the tuples (e.g. 10-tuples).

### Other Examples

Some examples courtesy of @fresheyeball.

```json
[{
   "foo"  : "foozle",
   "bars" : [{
       "zip" : [5, 3],
       "zop": null
   },{
       "zip":null,
       "zop":[1, 9, 2]
   }]
}]
```

```
|------------|---------------------------|
|            |            bars           |
|------------|---------------------------|
|    foo     |     zip     |     zop     |
|------------|-------------|-------------|
|   foozle   |           5 |             |
|            |           3 |             |
|            |-------------|-------------|
|            |             |           1 |
|            |             |           9 |
|            |             |           2 |
|------------|-------------|-------------|
```

```json
[{
    "foo": "foozle",
    "bars": [{
        "zip": 0,
        "zop": 5
      },{
        "zip": 3,
        "zop": 7
    }]
}]
```

```
|----------|-----------|
|          |    bars   |
|----------|-----------|
|   foo    | zip | zop |
|----------|-----|-----|
|  foozle  |  0  |  5  |
|          |-----|-----|
|          |  3  |  7  |
|----------|-----|-----|
```

```json
[{
    "foo": "foozle",
    "bars": [{
        "zip": 0,
        "zop": 5
      },{
        "zip": 3,
        "zop": 7
    }]
}, 
{
    "foo": "chozle",
    "bars": [{
        "zip": 7,
        "zop": 6
      },{
        "zip": 0,
        "zop": 2
    }]
},
{
    "foo": "boggle",
    "bars": [{
        "zip": 3,
        "zop": 8
      },{
        "zip": 3,
        "zop": 8
    }]
}]
```

```
|----------|-----------|
|          |    bars   |
|----------|-----------|
|   foo    | zip | zop |
|----------|-----|-----|
|  foozle  |  0  |  5  |
|          |-----|-----|
|          |  3  |  7  |
|----------|-----------|
|  chozle  |  7  |  6  |
|          |-----|-----|
|          |  0  |  2  |
|----------|-----------|
|  boggle  |  3  |  8  |
|          |-----|-----|
|          |  3  |  8  |
|----------|-----|-----|
```

**Note**: The schema is fully unified and the header appears only once. If objects are heterogeneous in structure, then this is reflected by many cells being blank (but the schema is still not repeated!).

## Examples

The [examples](/examples) directory contains a handful of example tables that are rendered with this library, as well as an editable text form that allows you to copy / paste JSON into the form and click *Render* to see how the library renders the JSON data.

## Tests

The library contains an extensive suite of unit tests written using [purescript-strongcheck](https://github.com/purescript-contrib/purescript-strongcheck), which verify correct translation of nested, heterogeneous, array-filled JSON data into clean tabular markup.

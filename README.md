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


## How?

Install with bower: `$ bower install purescript-jtable`


### Example Usage

See [examples/src/Main.purs](examples/src/Main.purs) for example usage.


### API

See [MODULES.md](MODULES.md).

The `Json` and `JCursor` data types that appear in the API are from
[purescript-argonaut](https://github.com/purescript-contrib/purescript-argonaut).
The `Markup` data type that appears in the API is a synonym for `HTML _ _`
from [purescript-halogen](https://github.com/slamdata/purescript-halogen).

Both of these libraries are listed as dependencies in [bower.json](bower.json).

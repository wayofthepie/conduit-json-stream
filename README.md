# Hacking
Some hacking about with the conduit and json-stream library.

## Working Example
Some notes so I remember... Given the following json in a file **simple.json**:
```json
{
    "a": {
        "b" : "c",
        "d" : {
            "e": "f"
        }
    }
}
```
I want to extract the value of **a** and **e**:
```haskell
-- | The parser would be as follows
p :: Parser (T.Text, T.Text)
p = (,) <$> "a" .: "b" .: string
        <*> "a" .: "d" .: "e" .: string

-- | And the conduit
jsonC "simple.json" p

-- | This gives
[("c","f")]
```

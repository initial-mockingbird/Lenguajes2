# JSON

## Grammar 

The context free grammar can be viewed in the [json official page](https://www.json.org/json-en.html)
(though, it can also be viewed in the `Free.hs` file provided that one reads the parser combinators as a grammar)

```BNF
json -> element

value -> object
      | array
      | string
      | number
      | "true"
      | "false"
      | "null"

object -> '{' ws '}'
        | '{' members '}'

members -> member 
        | member ',' members

member -> ws string ws ':' element 

array -> '[' ws ']'
      | '[' elements ']'

elements -> element 
        | element ',' elements

element -> ws value ws

string -> '"' characters '"'

characters -> ''
            | character characters

character -> '0020' . '10FFF' - '"' - '\' '\' escape 

escape -> '"'
        | '\'
        | '/'
        | 'b'
        | 'f'
        | 'n'
        | 'r'
        | 't'
        | 'u' hex hex hex hex 

hex -> digit 
    | 'A' . 'F'
    | 'a' . 'f'

number -> integer fraction exponent

integer -> digit 
        | onenine digits 
        | '-' digit 
        | '-' onenine digits 

digits -> digit
      | digit digits 

digit -> '0'
      | onenine 

onenive -> '1' . '9'

fraction -> ''
          | '.' digits 

exponent -> ''
          | 'E' sign digits 
          | 'e' sign digits 

sign -> ''
      | '+'
      | '-'

ws -> ''
    | '0020' ws
    | '000A' ws
    | '000D' ws
    | '0009' ws

```

# Dependencies (non-nix)

This project depends on haskell. Install via ghcup `ghc 9.10`, cabal and stack (we will only use hpack from the stack installation).

Optionally, it also uses [just](https://github.com/casey/just) to build and run. 

## Dependencies (nix)

This project is a flake. Just need to hop on `nix develop` to get the dependencies.

## Run (just)

Run `just parse file1 file2 file3...` to parse `file1,file2,file3...`. 
Each file can also contain blobs (but each blob must be expanded to a file). For example: `just parse ./jsons/*`

## Run 

If no `just`, then run the following command:

```bash
hpack; cabal run Json -- ./jsons/*
```


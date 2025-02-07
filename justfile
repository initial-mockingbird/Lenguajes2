default: parse
alias repl := ghci

build :
  hpack
  cabal build

ghci *args='':
  hpack
  cabal repl {{args}}

clean:
  cabal clean
  rm -rf ./dist/**

parse *args='./jsons/*': build 
  cabal run Json -- {{args}}
 


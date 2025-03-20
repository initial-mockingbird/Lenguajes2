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
 
tarea3:
  haddock -o ./Tarea3/ --html --base-url=. ./src/Tarea3.hs --hyperlinked-source

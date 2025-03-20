
{-|
Module      : Tarea3 
Description : Solucion de la tarea3 . 
Copyright   : (c) Daniel Pinto
                  15-11139
License     : GPL-3
Maintainer  : 15-11139@usb.ve 
Stability   : experimental
Portability : POSIX


-}
module Tarea3 (pregunta1a, pregunta2, getType, example, pregunta3a, pregunta3b, pregunta3d) where
import Control.Monad.State 
import Control.Monad
import GHC.Stack (HasCallStack)

{-| El grafo de tipos es el siguiente:

  ![diagrama 1.a](../diagrams/1a.png)

-}
pregunta1a :: String 
pregunta1a = mempty 

{-| El esquema de verificacion es:


 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | Expresion                                                               | Tipo                                                       | Substitucion                                        |
 +=========================================================================+============================================================+=====================================================+
 | \[cmap\]                                                                | \[\beta\]                                                  |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[f\]                                                                   | \[\gamma\]                                                 |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[x\]                                                                   | \[\rho\]                                                   |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[[]\]                                                                  | \[\forall a.list(a)\]                                      |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[null\]                                                                | \[\forall a.list(a) \rightarrow bool\]                     |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[head\]                                                                | \[\forall a. list(a) \rightarrow a\]                       |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[tail\]                                                                | \[\forall a. list(a) \rightarrow a\]                       |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[if\]                                                                  | \[\forall a. bool \times a \times a \rightarrow a\]        |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[concat\]                                                              | \[\forall a. list(a) \times list(a) \rightarrow list(a) \] |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[match\]                                                               | \[\forall a. a \times a \rightarrow a\]                    |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[cmap(f,x)\]                                                           | \[a_1\]                                                    | \[\beta = \gamma \rightarrow \rho \rightarrow a_1\] |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[head(x)\]                                                             | \[a_2\]                                                    | \[\rho = list(a_2)\]                                |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[tail(x)\]                                                             | \[list(a_2)\]                                              |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[cmap(f,tail(x))\]                                                     | \[a_1\]                                                    |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[f(head(x))\]                                                          | \[a_3\]                                                    | \[\gamma = a_2 \rightarrow a_3\]                    |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[concat(f(head(x)),cmap(f,tail(x)))\]                                  | \[list(a_4)\]                                              |\[a_3 = list(a_4), a_1 = list(a_4) \]                |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[null(x)\]                                                             | \[bool\]                                                   |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[if(null(x),[],concat(f(head(x)),cmap(f,tail(x))))\]                   | \[list(a_4)\]                                              |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+
 | \[match(cmap(f,x), if(null(x),[],concat(f(head(x)),cmap(f,tail(x)))))\] | \[list(a_4)\]                                              |                                                     |
 +-------------------------------------------------------------------------+------------------------------------------------------------+-----------------------------------------------------+

 Es decir:
 \[\begin{aligned}
   \beta &= \gamma \rightarrow \rho \rightarrow a_1 \\
         &= (a_2 \rightarrow a_3)\rightarrow list(a_2) \rightarrow a_1 \\ 
         &= (a_2 \rightarrow list(a_4))\rightarrow list(a_2) \rightarrow list(a_4) \\
         &= \forall a_2\ a_4. (a_2 \rightarrow list(a_4))\rightarrow list(a_2) \rightarrow list(a_4)
   \end{aligned}
 \]

 El cual es el tipo mas general de \[concatMap\]

-}
pregunta2 :: String
pregunta2 = mempty

infix 4 :<:
infix 3 :?:
data E  
  = E  :+: E 
  | E  :^: E 
  | E  :<: E 
  | E  :?: E 
  | Force (E )
  | Paren (E )
  | ENum Int 
  | EBool Bool 
  | Null 

data Types 
  = MyInt  
  | MyBool   
  | MyNull  
  | MyVoid  
  | MyBottom deriving (Eq)



instance Semigroup Types where 
  MyBottom <> _   = MyBottom 
  _ <> MyBottom   = MyBottom 
  MyNull <> _     = MyNull
  _ <> MyNull     = MyNull
  a <> b | a == b = a
  _ <> _          = MyBottom



data Rose a = MkRose a [Rose a] deriving Show

getType ::  E -> Types 
getType (a :+: b)  = MyInt <> getType  a <> getType  b
getType (a :^: b)  = MyBool <> getType  a <> getType  b
getType (a :<: b)  = case (getType a, getType b) of 
  (MyInt,MyInt) -> MyBool 
  (a',b') | a' <> b' == MyNull -> MyNull
  _ -> MyBottom
getType (a :?: b)  = if getType  a == MyNull then getType  b else getType  a
getType (Force a)  = if getType  a == MyNull then MyBottom else getType  a 
getType (Paren a)  = getType  a
getType (ENum  _)  = MyInt 
getType (EBool _)  = MyBool 
getType Null       = MyNull 


getDerivationTree :: E -> [Rose (String,Types)]
getDerivationTree  e@(a :+: b) = let 
  ta = getDerivationTree  a
  tb = getDerivationTree  b
  tab = ta <> tb
  te = getType  e 
  in [MkRose (show e,te) [ta !! 0, tb !!0]] <> tab
getDerivationTree  e@(a :^: b) = let 
  ta = getDerivationTree  a
  tb = getDerivationTree  b
  tab = ta <> tb
  te = getType  e 
  in [MkRose (show e,te) [ta !! 0, tb !!0]] <> tab
getDerivationTree  e@(a :<: b) = let 
  ta = getDerivationTree  a
  tb = getDerivationTree  b
  tab = ta <> tb
  te = getType  e 
  in [MkRose (show e,te) [ta !! 0, tb !!0]] <> tab
getDerivationTree  e@(a :?: b) = let 
  ta = getDerivationTree  a
  tb = getDerivationTree  b
  tab = ta <> tb
  te = getType  e 
  in [MkRose (show e,te) [ta !! 0, tb !!0]] <> tab
getDerivationTree  e@(Force a) = let 
  ta = getDerivationTree  a
  te = getType  e
  in  [MkRose (show e, te) [ta !! 0]] <> ta
getDerivationTree  e@(Paren a) = let 
  ta = getDerivationTree  a
  te = getType  e
  in [MkRose (show e, te) [ta !! 0] ] <> ta 
getDerivationTree  e@(ENum  _) = [MkRose (show e,getType  e) []]
getDerivationTree  e@(EBool _) = [MkRose (show e,getType  e) []]
getDerivationTree  e@Null      = [MkRose (show e,getType  e) []]


example :: E 
example = Paren (Paren (Null :?: 42) :+: Paren (Force 69)) :<: Paren (Paren (7 :?: Null) :^: Null) :?: EBool True



{-| La funcion `getType` obtiene el tipo de una expresion usando solo atributos sintetizados. Vea el
 - source para la implementacion.
-}
pregunta3a :: E -> Types 
pregunta3a = getType 

{-| El tipo de la frase, y como se obtiene, se puede ver en los siguientes arboles:
 
  ![diagrama 3.b](../diagrams/3a.svg)

 
-}
pregunta3b :: [Rose (String,Types )]
pregunta3b = getDerivationTree  example


{-| La pregunta 3.a planteada como regla de inferencia:

  @
    G |- e0 : Int  G |- e1 : Int
    -----------------------------
          G |- e0 + e1 : Int

    G |- e0 : Bool  G |- e1 : Bool
    -----------------------------
         G |- e0 ^ e1 :  Bool

    G |- e0 : Int  G |- e1 : Int
    -----------------------------
         G |- e0 < e1 : Bool 

      G |- e0 : T   T /= Null
    -----------------------------
         G |- e0 ?: e1 : T

     G |- e0 : Null G |- e1 : T
    -----------------------------
         G |- e0 ?: e1 : T


       G |- e0 : T   T /= Null
    -----------------------------
         G |- e0!! : T

           G |- e0 : Null 
    -----------------------------
         G |- e0!! : Bottom

     G |- e : T
    -------------
    G |- (e) : T

    ---------------
    G |- num : Int 

    -----------------
    G |- true : Bool

    -----------------
    G |- false : Bool 

    -----------------
    G |- null : Null 
  @

-}
pregunta3d :: String 
pregunta3d = mempty 

-----------------
--Aux functions 
-----------------

type MermaidID = Int 
type Node      = String 

toMermaid :: HasCallStack => FilePath -> [Rose String] -> IO ()
toMermaid fp rs  
  = writeFile fp 
  $ "%%{init: {\"flowchart\": {\"htmlLabels\": false}} }%%\n"
  <> "flowchart TD\n" 
  <> evalState subgraphs 0
  where 
    toMermaid' :: HasCallStack => Rose String -> State MermaidID (String,[Node])
    toMermaid' (MkRose x xs) = do 
      mid <- get 
      let node  = "x" <> show mid
      let node' = "  " <> node <> "[\"" <> x <> "\"]"
      let mkNode aNode = "    " <> node' <> " --> " <> aNode <> "\n"
      put $ mid + 1
      mmds <- forM xs $ \r -> do 
        (acc,nodes) <- toMermaid' r 
        let acc' = concatMap mkNode nodes 
        pure $ acc' <> acc
      pure (concat mmds, [node'])
    
    subgraphs :: HasCallStack => State MermaidID String
    subgraphs = do 
      gs  <- fmap fst <$> traverse toMermaid' rs 
      gs' <- forM gs $ \g -> if g == "" then pure "" else do 
        mid <- get 
        put $ mid + 1
        pure $ "  subgraph " <> "n" <> show mid <> " [.]\n" <> g <> "  end\n"
      pure $ concat gs'

gen3a :: IO ()
gen3a = toMermaid "./diagrams/3a.mmd" $ f pregunta3b
  where 
    f :: HasCallStack => [Rose (String,Types)] -> [Rose String] 
    f roses = [MkRose (s <> " : " <> show t) $ f  rs | MkRose (s,t) rs <- roses]

instance Show Types where 
  show MyInt  = "Int"
  show MyBool = "Bool"
  show MyNull = "Null"
  show MyVoid = "Void"
  show MyBottom = "⊥"

instance Num E where 
  fromInteger = ENum . fromIntegral
  _ + _    = error "not implemented"
  _ * _    = error "not implemented"
  signum _ = error "not implemented"
  negate _ = error "not implemented"
  abs _    = error "not implemented"



instance Show E where 
  show (a :+: b) = show a <> " + " <> show b
  show (a :^: b) = show a <> " ^ " <> show b
  show (a :<: b) = show a <> " < " <> show b
  show (a :?: b) = show a <> " ?: " <> show b
  show (Force e) = show e <> "!!"
  show (Paren e) = "(" <> show e <> ")"
  show (ENum n)  = show n
  show (EBool b) = show b
  show Null      = "null"


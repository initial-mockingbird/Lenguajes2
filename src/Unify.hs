{-# Language PatternSynonyms #-}
{-# Language LambdaCase      #-}
module Unify where 

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.RWS
import Control.Monad.Except
import Data.List (intercalate)

type Name = String 


newtype TVar = TV Name deriving (Eq,Ord)
data Type
  = TCon Name [Type]
  | TVar TVar
  deriving (Eq)

instance Show TVar where
  show (TV n) = n

instance Show Type where 
  show (TVar t)    = show t
  show (TCon n []) = n
  show (TCon n xs) = n <> "(" <> intercalate "," (show <$> xs)  <> ")"


pattern TList a = TCon "list" [a]
pattern TBool   = TCon "bool" []
pattern a :-> b = TCon "->" [a, b]

data Scheme = Forall (Set.Set TVar) Type
data Constraint = Constraint Type Type
type Context = Map.Map Name Scheme
type Constraints = [Constraint]
type Subst = Map.Map TVar Type
type Alphabet = [Name]

greekAlphabet :: [Name]
greekAlphabet = let 
  as  = [[x] | x <- "ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω"] <> as
  as' = [x <> show n | (x,n) <- as `zip` [(1 :: Int)..]]
  in as'

data WriterST = WST 
  { constraints :: Constraints 
  , logs        :: String
  }

instance Semigroup WriterST where 
  (WST x0 y0) <> (WST x1 y1) = WST {constraints=x0 <> x1,logs=y0 <> y1}

instance Monoid WriterST where 
  mempty = WST mempty mempty

type Infer a = RWST Context WriterST Alphabet (Except String) a

constrain :: Type -> Type -> Infer ()
constrain x y = tell $ WST [Constraint x y] $ "[Constraint] => " <> show x <> "~" <> show y <> "\n"

fresh :: Infer Type
fresh = do
  alphabet <- get
  put (drop 1 alphabet)
  return . TVar . TV $ head alphabet

compose :: Subst -> Subst -> Subst
compose a b = Map.map (apply a) (b `Map.union` a)

class Substitutable a where
  apply :: Subst -> a -> a
  tvs :: a -> Set.Set TVar

instance Substitutable Type where
  tvs (TVar tv) = Set.singleton tv
  tvs (TCon _ ts) = foldr (Set.union . tvs) Set.empty ts
  apply s t@(TVar tv) = Map.findWithDefault t tv s
  apply s (TCon c ts) = TCon c $ apply s <$> ts

instance Substitutable Scheme where
  tvs (Forall vs t) = tvs t `Set.difference`  vs
  apply s (Forall vs t) = Forall vs $ apply (foldr Map.delete s vs) t
  
instance Substitutable Constraint where
  tvs (Constraint t1 t2) = tvs t1 `Set.union` tvs t2
  apply s (Constraint t1 t2) = Constraint (apply s t1) (apply s t2)
  
instance Substitutable a => Substitutable [a] where
  tvs l = foldr (Set.union . tvs) Set.empty l
  apply s = map (apply s)



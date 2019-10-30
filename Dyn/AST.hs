module Dyn.AST where

import Debug.Trace

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set  as S

traceShowX v id = traceShow (v, "==>", id) id
traceShowS v = traceShow (toString v) v
traceShowSS vs = traceShow (concatMap (++"\n") $ map toString vs) vs

type Pos = (Int,Int)
pz = (0,0)

type ID_Var  = String
type ID_Data = String
type ID_Hier = [ID_Data]
type ID_Ifce = String

-------------------------------------------------------------------------------

class IPos a where
  getPos :: a -> Pos

class IList a where
  toList   ::  a  -> [a]
  fromList :: [a] ->  a

class IString a where
  toString  :: a -> String
  toStringI :: Int -> a -> String

-------------------------------------------------------------------------------

newtype Type = Type (Pos, TType, TCtrs)
  deriving (Eq,Show)

cz = []
tz = Type (pz, TAny, cz)

type TCtr  = (ID_Var, [ID_Ifce])  -- (a,[IEq,IOrd,IShow])
type TCtrs = [TCtr]               -- [(a,[IEq,IOrd,IShow]), (b,[...])]

data TType = TAny
           | TUnit
           | TVar   ID_Var
           | TData  ID_Hier {-[TType]-}       -- X.Y of (Int,Bool) // data X.Y of (a,b) with (a,b)
           | TTuple [TType]               -- (len >= 2)
           | TFunc  {-FuncType-} TType TType  -- inp out
  deriving (Eq,Show)

ctrsToMap :: TCtrs -> M.Map ID_Var (S.Set ID_Ifce)
ctrsToMap cs = M.map S.fromAscList $ M.fromAscList cs

ctrsFromMap :: M.Map ID_Var (S.Set ID_Ifce) -> TCtrs
ctrsFromMap csmap = map (\(k,v)->(k,S.toAscList v)) $ M.toAscList csmap

-------------------------------------------------------------------------------

data Expr
  = EError Pos String                 -- (msg)        -- error "bug found"
  | EVar   Pos ID_Var                 -- (id)         -- a ; xs
  | EUnit  Pos                        -- ()           -- ()
  | ECons  Pos ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | EData  Pos ID_Hier Expr           -- (ids,struct) -- B.True () ; Int.1 () ; T.Node (T.Leaf(),T.Leaf())
  | ETuple Pos [Expr]                 -- (exprs)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | EFunc  Pos Type Ups ExpWhere      -- (type,ups,body)
  | ECall  Pos Expr Expr              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  | EArg   Pos
  | ECase  Pos Expr [(Patt,ExpWhere)] -- (exp,[(pat,whe)] -- case x of A->a B->b _->z
  deriving (Eq, Show)

type Ups = [(ID_Var,Expr)]            -- [(x,1),(y,())]

-------------------------------------------------------------------------------

data Patt
  = PError Pos String                 -- (msg)        -- error "bug found"
  | PArg   Pos                        -- ()           -- ...
  | PAny   Pos                        -- ()           -- _
  | PWrite Pos ID_Var                 -- (id)         -- =a ; =xs
  | PRead  Pos Expr                   -- (exp)        -- ~a ; ~xs
  | PUnit  Pos                        -- ()           -- ()
  | PCons  Pos ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | PTuple Pos [Patt]                 -- (patts)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | PCall  Pos Patt Patt              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  deriving (Eq, Show)

-------------------------------------------------------------------------------

newtype ExpWhere = ExpWhere (Pos, Expr, [Decl])
  deriving (Eq, Show)

data Decl = DSig Pos ID_Var Type
          | DAtr Pos Patt ExpWhere
  deriving (Eq, Show)

newtype Ifce = Ifce (Pos, ID_Ifce, TCtrs, [Decl])
  deriving (Eq, Show)

newtype Impl = Impl (Pos, ID_Ifce, Type, [Decl])
  deriving (Eq, Show)

newtype Prog = Prog [Glob]

-- top-level global declarations
data Glob = GDecl Decl | GIfce Ifce | GImpl Impl

-------------------------------------------------------------------------------

isDSig :: Decl -> Bool
isDSig (DSig _ _ _) = True
isDSig _            = False

isDAtr :: Decl -> Bool
isDAtr (DAtr _ _ _) = True
isDAtr _            = False

isEError (EError _ _) = True
isEError _            = False

declsSplit :: [Decl] -> ([Decl],[Decl])
declsSplit decls = (filter isDSig decls, filter isDAtr decls)

globToDecl :: Glob -> Decl
globToDecl (GDecl decl) = decl
  -- refuse GIfce/GImpl

globFromDecl :: Decl -> Glob
globFromDecl decl = GDecl decl

module Dyn.AST where

import Debug.Trace

import qualified Data.List as L
--import qualified Data.Map  as M
import qualified Data.Set  as S

traceShowX v id = traceShow (v, "==>", id) id
traceShowS v = traceShow (toString v) v
traceShowSS vs = traceShow (concatMap (++"\n") $ map toString vs) vs

type Pos = (Int,Int)
pz :: Pos
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

class IType a where
  toType :: [Decl] -> a -> Type

-------------------------------------------------------------------------------

{-
type    Ctr  = (ID_Var, [ID_Ifce])      -- (a,[IEq,IOrd,IShow])
newtype Ctrs = Ctrs { getCtrs::[Ctr] }  -- [(a,[IEq,IOrd,IShow]), (b,[...])]
  deriving (Show,Eq)
-}

newtype Ctrs = Ctrs { getCtrs::[ID_Ifce] }  -- [IEq,IOrd,IShow]
  deriving (Show,Eq)
cz = Ctrs []

data Type = TAny
          | TUnit
          | TVar   ID_Var
          | TData  ID_Hier {-[Type]-}       -- X.Y of (Int,Bool) // data X.Y of (a,b) with (a,b)
          | TTuple [Type]               -- (len >= 2)
          | TFunc  {-FuncType-} Type Type  -- inp out
  deriving (Eq,Show)

ctrsToSet :: Ctrs -> S.Set ID_Ifce
ctrsToSet (Ctrs cs) = S.fromAscList cs

ctrsFromSet :: S.Set ID_Ifce -> Ctrs
ctrsFromSet csset = Ctrs $ S.toAscList csset

{-
ctrsToMap :: Ctrs -> M.Map ID_Var (S.Set ID_Ifce)
ctrsToMap (Ctrs cs) = M.map S.fromAscList $ M.fromAscList cs

ctrsFromMap :: M.Map ID_Var (S.Set ID_Ifce) -> Ctrs
ctrsFromMap csmap = Ctrs $ map (\(k,v)->(k,S.toAscList v)) $ M.toAscList csmap
-}

-------------------------------------------------------------------------------

data Expr
  = EError Pos String                 -- (msg)        -- error "bug found"
  | EArg   Pos
  | EUnit  Pos                        -- ()           -- ()
  | EVar   Pos ID_Var                 -- (id)         -- a ; xs
  | ECons  Pos ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | ETuple Pos [Expr]                 -- (exprs)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | EFunc  Pos Ctrs Type Ups ExpWhere -- (type,ups,body)
  | ECall  Pos Expr Expr              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  | ECase  Pos Expr [(Patt,ExpWhere)] -- (exp,[(pat,whe)] -- case x of A->a B->b _->z
  | EData  Pos ID_Hier Expr           -- (ids,struct) -- B.True () ; Int.1 () ; T.Node (T.Leaf(),T.Leaf())
  | EType  Pos Type
  deriving (Eq, Show)

type Ups = [(ID_Var,Expr)]            -- [(x,1),(y,())]

-------------------------------------------------------------------------------

data Patt
  = PError Pos String                 -- (msg)        -- error "bug found"
  | PArg   Pos                        -- ()           -- ...
  | PAny   Pos                        -- ()           -- _
  | PUnit  Pos                        -- ()           -- ()
  | PCons  Pos ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | PWrite Pos ID_Var                 -- (id)         -- =a ; =xs
  | PRead  Pos Expr                   -- (exp)        -- ~a ; ~xs
  | PTuple Pos [Patt]                 -- (patts)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | PCall  Pos Patt Patt              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  deriving (Eq, Show)

-------------------------------------------------------------------------------

newtype ExpWhere = ExpWhere (Pos, [Decl], Expr)
  deriving (Eq, Show)

data Decl = DSig Pos ID_Var Ctrs Type
          | DAtr Pos Patt ExpWhere
  deriving (Eq, Show)

newtype Ifce = Ifce (Pos, ID_Ifce, Ctrs, [Decl])
  deriving (Eq, Show)

newtype Impl = Impl (Pos, ID_Ifce, Ctrs, Type, [Decl])
  deriving (Eq, Show)

newtype Prog = Prog [Glob]

-- top-level global declarations
data Glob = GDecl Decl | GIfce Ifce | GImpl Impl

-------------------------------------------------------------------------------

isDSig :: Decl -> Bool
isDSig (DSig _ _ _ _) = True
isDSig _              = False

isDAtr :: Decl -> Bool
isDAtr (DAtr _ _ _) = True
isDAtr _            = False

isEError (EError _ _) = True
isEError _            = False

globToDecl :: Glob -> Decl
globToDecl (GDecl decl) = decl
  -- refuse GIfce/GImpl

globFromDecl :: Decl -> Glob
globFromDecl decl = GDecl decl

dsigsFind :: [Decl] -> ID_Var -> (Ctrs,Type)
dsigsFind dsigs id = case L.find f dsigs of
                      Nothing               -> (cz,TAny)
                      Just (DSig _ _ cs tp) -> (cs,tp)
                     where
                      f :: Decl -> Bool
                      f (DSig _ x _ _) = (id == x)

-------------------------------------------------------------------------------

type MapFs = ( ([Ifce]->Ctrs->[Decl]->Decl->[Decl]),
               ([Ifce]->Ctrs->[Decl]->Expr->Expr  ),
               ([Ifce]->Ctrs->[Decl]->Patt->Patt) )
fDz _ _ _ d = [d]
fEz _ _ _ e = e
fPz _ _ _ p = p

mapDecls :: MapFs -> [Ifce] -> Ctrs -> [Decl] -> [Decl] -> [Decl]
mapDecls fs ifces ctrs dsigs decls = concatMap (mapDecl fs ifces ctrs dsigs') decls
  where
    dsigs' = dsigs ++ filter isDSig decls

mapDecl :: MapFs -> [Ifce] -> Ctrs -> [Decl] -> Decl -> [Decl]
mapDecl fs@(fD,_,_) ifces ctrs dsigs decl@(DSig _ _ _ _) = fD ifces ctrs dsigs decl
mapDecl fs@(fD,_,_) ifces ctrs dsigs (DAtr z pat whe)    = fD ifces ctrs dsigs $ DAtr z pat' whe'
  where
    pat' = mapPatt  fs ifces ctrs dsigs pat
    whe' = mapWhere fs ifces ctrs dsigs whe

mapWhere :: MapFs -> [Ifce] -> Ctrs -> [Decl] -> ExpWhere -> ExpWhere
mapWhere fs ifces ctrs dsigs (ExpWhere (z,ds,e)) = ExpWhere (z, ds', e')
  where
    e'  = mapExpr  fs ifces ctrs dsigs'' e
    ds' = mapDecls fs ifces ctrs dsigs'  ds

    dsigs''   = dsigs ++ filter isDSig ds'
    dsigs'    = dsigs ++ filter isDSig ds

mapPatt :: MapFs -> [Ifce] -> Ctrs -> [Decl] -> Patt -> Patt
mapPatt fs@(_,_,fP) ifces ctrs dsigs p = fP ifces ctrs dsigs (aux p) where
  aux (PRead  z e)     = PRead  z $ mapExpr fs ifces ctrs dsigs e
  aux (PTuple z ps)    = PTuple z $ map (mapPatt fs ifces ctrs dsigs) ps
  aux (PCall  z p1 p2) = PCall  z p1' p2' where
                          p1' = mapPatt fs ifces ctrs dsigs p1
                          p2' = mapPatt fs ifces ctrs dsigs p2
  aux p                = p

mapExpr :: MapFs -> [Ifce] -> Ctrs -> [Decl] -> Expr -> Expr
mapExpr fs@(_,fE,_) ifces ctrs dsigs e = fE ifces ctrs dsigs (aux e) where
  aux (EFunc  z cs tp ups whe) = EFunc  z cs tp ups $ mapWhere fs ifces ctrs' (arg:dsigs) whe where
                                  ctrs' = Ctrs $ getCtrs ctrs ++ getCtrs cs
                                  arg   = DSig z "..." cz tp' where
                                            tp' = case tp of
                                                    TFunc inp _ -> inp
                                                    otherwise   -> TAny
  aux (EData  z hr e)          = EData  z hr $ mapExpr fs ifces ctrs dsigs e
  aux (ETuple z es)            = ETuple z $ map (mapExpr fs ifces ctrs dsigs) es
  aux (ECall  z e1 e2)         = ECall  z e1' e2' where
                                  e1' = mapExpr fs ifces ctrs dsigs e1
                                  e2' = mapExpr fs ifces ctrs dsigs e2
  aux (ECase  z e l)           = ECase  z e' l' where
                                  e'       = mapExpr fs ifces ctrs dsigs e
                                  (ps, ws) = unzip l
                                  ps'      = map (mapPatt  fs ifces ctrs dsigs) ps
                                  ws'      = map (mapWhere fs ifces ctrs dsigs) ws
                                  l'       = zip ps' ws'
  aux e                        = e

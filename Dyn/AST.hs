module Dyn.AST where

import Debug.Trace

import qualified Data.List as L
import qualified Data.Map  as M
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

type    Ctr  = (ID_Var, [ID_Ifce])  -- (a,[IEq,IOrd,IShow])
newtype Ctrs = Ctrs [Ctr]           -- [(a,[IEq,IOrd,IShow]), (b,[...])]
  deriving (Show,Eq)
cz = Ctrs []

data Type = TAny
          | TUnit
          | TVar   ID_Var
          | TData  ID_Hier {-[Type]-}       -- X.Y of (Int,Bool) // data X.Y of (a,b) with (a,b)
          | TTuple [Type]               -- (len >= 2)
          | TFunc  {-FuncType-} Type Type  -- inp out
  deriving (Eq,Show)

ctrsToMap :: Ctrs -> M.Map ID_Var (S.Set ID_Ifce)
ctrsToMap (Ctrs cs) = M.map S.fromAscList $ M.fromAscList cs

ctrsFromMap :: M.Map ID_Var (S.Set ID_Ifce) -> Ctrs
ctrsFromMap csmap = Ctrs $ map (\(k,v)->(k,S.toAscList v)) $ M.toAscList csmap

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
  | EType Pos Type
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

newtype ExpWhere = ExpWhere (Pos, Expr, [Decl])
  deriving (Eq, Show)

data Decl = DSig Pos ID_Var Type
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

dsigFind :: [Decl] -> ID_Var -> Type
dsigFind dsigs id = case L.find f dsigs of
                      Nothing            -> TAny
                      Just (DSig _ _ tp) -> tp
                    where
                      f :: Decl -> Bool
                      f (DSig _ x _) = (id == x)

-------------------------------------------------------------------------------

type MapFs = ( ([Ifce]->[Decl]->Decl->[Decl]),
               ([Ifce]->[Decl]->Expr->([Decl],Expr)),
               ([Ifce]->[Decl]->Patt->Patt) )
fDz _ _ d = [d]
fEz _ _ e = ([],e)
fPz _ _ p = p

mapDecls :: MapFs -> [Ifce] -> [Decl] -> [Decl] -> [Decl]
mapDecls fs ifces dsigs decls = concatMap (mapDecl fs ifces dsigs') decls
  where
    dsigs' = dsigs ++ filter isDSig decls

mapDecl :: MapFs -> [Ifce] -> [Decl] -> Decl -> [Decl]
mapDecl fs@(fD,_,_) ifces dsigs decl@(DSig _ _ _) = fD ifces dsigs decl
mapDecl fs@(fD,_,_) ifces dsigs (DAtr z pat whe)  = (fD ifces dsigs $ DAtr z pat' whe') ++ dsPat'
  where
    (dsPat',pat') = mapPatt  fs ifces dsigs pat
    whe'          = mapWhere fs ifces dsigs whe

mapWhere :: MapFs -> [Ifce] -> [Decl] -> ExpWhere -> ExpWhere
mapWhere fs ifces dsigs (ExpWhere (z,e,ds)) = ExpWhere (z, e', dsE'++ds')
  where
    dsigs'    = dsigs ++ filter isDSig ds
    (dsE',e') = mapExpr  fs ifces dsigs' e
    ds'       = mapDecls fs ifces dsigs' ds

mapPatt :: MapFs -> [Ifce] -> [Decl] -> Patt -> ([Decl], Patt)
mapPatt fs@(_,_,fP) ifces dsigs p = (dsP', fP ifces dsigs p') where
  (dsP',p') = aux p
  aux (PRead  z e)     = (dsE', PRead  z $ e') where
                          (dsE',e') = mapExpr fs ifces dsigs e
  aux (PTuple z ps)    = (concat dsPs', PTuple z ps') where
                          (dsPs',ps') = unzip $ map (mapPatt fs ifces dsigs) ps
  aux (PCall  z p1 p2) = (dsP1'++dsP2', PCall  z p1' p2') where
                          (dsP1',p1') = mapPatt fs ifces dsigs p1
                          (dsP2',p2') = mapPatt fs ifces dsigs p2
  aux p                = ([], p)

mapExpr :: MapFs -> [Ifce] -> [Decl] -> Expr -> ([Decl], Expr)
mapExpr fs@(_,fE,_) ifces dsigs e = (dsE'++dsE'', e'') where
  (dsE'',e'') = fE ifces dsigs e'
  (dsE', e')  = aux e
  aux (EFunc  z tp cs ups whe) = ([], EFunc z tp cs ups $ mapWhere fs ifces dsigs whe)
  aux (EData  z hr e)       = (dsE', EData z hr e') where
                                (dsE',e') = mapExpr fs ifces dsigs e
  aux (ETuple z es)         = (concat dsEs', ETuple z es') where
                                (dsEs',es') = unzip $ map (mapExpr fs ifces dsigs) es
  aux (ECall  z e1 e2)      = (dsE1'++dsE2', ECall  z e1' e2') where
                                (dsE1',e1') = mapExpr fs ifces dsigs e1
                                (dsE2',e2') = mapExpr fs ifces dsigs e2
  aux (ECase  z e l)        = (dsE'++concat dsPs', ECase z e' l') where
                                (dsE', e')  = mapExpr fs ifces dsigs e
                                (ps, ws)    = unzip l
                                (dsPs',ps') = unzip $ map (mapPatt fs ifces dsigs) ps
                                ws'         = map (mapWhere fs ifces dsigs) ws
                                l'          = zip ps' ws'
  aux e                     = ([], e)

module Dyn.AST where

import Debug.Trace

import qualified Data.List as L

data Ann = Ann { pos :: (Int,Int) }
  deriving (Eq, Show)

az = Ann { pos=(0,0) }

type ID_Var  = String
type ID_Data = String
type ID_Hier = [ID_Data]
type ID_Ifce = String

-------------------------------------------------------------------------------

class IAnn a where
  getAnn :: a -> Ann

class IList a where
  toList   ::  a  -> [a]
  fromList :: [a] ->  a

class IString a where
  toString  :: a -> String
  toStringI :: Int -> a -> String

-------------------------------------------------------------------------------

newtype Type = Type (Ann, TType, TCtrs)
  deriving (Eq,Show)

cz = []
tz = Type (az, TUnit, cz)

type TCtr  = (ID_Var, [ID_Ifce])  -- (a,[IEq,IOrd,IShow])
type TCtrs = [TCtr]               -- [(a,[IEq,IOrd,IShow]), (b,[...])]

data TType = --TAny                         -- bot/sup
             TUnit
           | TVar   ID_Var
           | TData  ID_Hier {-[TType]-}       -- X.Y of (Int,Bool) // data X.Y of (a,b) with (a,b)
           | TTuple [TType]               -- (len >= 2)
           | TFunc  {-FuncType-} TType TType  -- inp out
  deriving (Eq,Show)

-------------------------------------------------------------------------------

data Expr
  = EError Ann String                 -- (msg)        -- error "bug found"
  | EVar   Ann ID_Var                 -- (id)         -- a ; xs
  | EUnit  Ann                        -- ()           -- ()
  | ECons  Ann ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | EData  Ann ID_Hier Expr           -- (ids,struct) -- B.True () ; Int.1 () ; T.Node (T.Leaf(),T.Leaf())
  | ETuple Ann [Expr]                 -- (exprs)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | EFunc  Ann Type Ups Where         -- (type,ups,body)
  | ECall  Ann Expr Expr              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  | EArg   Ann
  | ECase  Ann Expr [(Patt,Where)]    -- (exp,[(pat,whe)] -- case x of A->a B->b _->z
  deriving (Eq, Show)

type Ups = [(ID_Var,Expr)]            -- [(x,1),(y,())]

-------------------------------------------------------------------------------

data Patt
  = PError Ann String                 -- (msg)        -- error "bug found"
  | PArg   Ann                        -- ()           -- ...
  | PAny   Ann                        -- ()           -- _
  | PWrite Ann ID_Var                 -- (id)         -- =a ; =xs
  | PRead  Ann Expr                   -- (exp)        -- ~a ; ~xs
  | PUnit  Ann                        -- ()           -- ()
  | PCons  Ann ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | PTuple Ann [Patt]                 -- (patts)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | PCall  Ann Patt Patt              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  deriving (Eq, Show)

-------------------------------------------------------------------------------

newtype Where = Where (Ann, Expr, [Decl])
  deriving (Eq, Show)

data Decl = DSig Ann ID_Var Type
          | DAtr Ann Patt Where
  deriving (Eq, Show)

newtype Ifce = Ifce (Ann, ID_Ifce, TCtrs, [Decl])
  deriving (Eq, Show)

newtype Impl = Impl (Ann, (ID_Ifce,ID_Hier), [Decl])
  deriving (Eq, Show)

type Prog = Where

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

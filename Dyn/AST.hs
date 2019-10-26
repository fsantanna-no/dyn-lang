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

newtype Type = Type (Ann, TType, TCtrs)
  deriving (Eq,Show)

cz = []
tz = Type (az, TUnit, cz)

type TCtrs = [(ID_Var, [ID_Ifce])]  -- [(a,[IEq,IOrd,IShow]), (b,[...])]

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

newtype Ifce = Ifce (Ann, (ID_Ifce,ID_Var), [Decl])
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

-------------------------------------------------------------------------------

-- Classes.hs

class IAnn a where
  getAnn :: a -> Ann

class IList a where
  toList   ::  a  -> [a]
  fromList :: [a] ->  a

class IString a where
  toString  :: a -> String
  toStringI :: Int -> a -> String

-------------------------------------------------------------------------------

rep spc = replicate spc ' '

-------------------------------------------------------------------------------

instance IString Type where
  toString (Type (_,ttp,cs)) =
    case cs of
      [] -> toString ttp
      l  -> toString ttp ++ " where (" ++ L.intercalate "," (map f l) ++ ")" where
              f (var,[cls]) = var ++ " is " ++ cls
              f (var,clss)  = var ++ " is (" ++ L.intercalate "," clss ++ ")"
  toStringI _ = error "TODO"


-------------------------------------------------------------------------------

instance IString TType where
  --toString TAny            = "?"
  toString TUnit            = "()"
  toString (TVar   id)      = id
  toString (TData  hr)      = L.intercalate "." hr
  toString (TTuple ttps)    = "(" ++ L.intercalate "," (map toString ttps) ++ ")"
  toString (TFunc  inp out) = "(" ++ toString inp ++ " -> " ++ toString out ++ ")"
  --toString (TData ids [x]) = L.intercalate "." ids ++ " of " ++ toString x
  --toString (TData ids ofs) = L.intercalate "." ids ++ " of " ++ "(" ++ L.intercalate "," (map toString ofs) ++ ")"

  toStringI _ = error "TODO"

-------------------------------------------------------------------------------

instance IString Expr where
  toString expr = toStringI 0 expr

  toStringI spc (EError z msg)         = "(line=" ++ show ln ++ ", col=" ++ show cl ++ ") ERROR : " ++ msg
                                              where (ln,cl) = pos z
  toStringI spc (EVar   _ id)          = id
  toStringI spc (EUnit  _)             = "()"
  toStringI spc (ECons  _ h)           = L.intercalate "." h
  toStringI spc (EData  _ h (EUnit _)) = L.intercalate "." h
  toStringI spc (EData  _ h st)        = "(" ++ L.intercalate "." h ++ " " ++ toString st ++ ")"
  toStringI spc (EArg   _)             = "..."
  toStringI spc (ETuple _ es)          = "(" ++ L.intercalate "," (map toString es) ++ ")"
  toStringI spc (EFunc  _ tp ups bd)   = "func :: " ++ toString tp ++ " " ++ upsToString ups ++"->\n" ++ rep (spc+2) ++
                                              toStringI (spc+2) bd ++ "\n" ++ rep spc ++ ";"
                                             where
                                              upsToString []  = ""
                                              upsToString ups = "{" ++ (L.intercalate "," $ map fst ups) ++ "} "
  toStringI spc (ECall  _ e1 e2)       = "(" ++ toString e1 ++ " " ++ toString e2 ++ ")"

  toStringI spc (ECase  _ e cases)     =
    "case " ++ toString e ++ " of" ++ concat (map f cases) ++ "\n" ++ rep spc ++ ";"
    where
      f :: (Patt,Where) -> String
      f (pat,whe) = "\n" ++ rep (spc+2) ++ toString pat ++ " -> " ++ toStringI (spc+2) whe
  --toStringI e                    = error $ show e

-------------------------------------------------------------------------------

instance IString Patt where
  toString patt = toStringI 0 patt

  toStringI spc (PArg   _)           = "..."
  toStringI spc (PAny   _)           = "_"
  toStringI spc (PWrite _ id)        = {-"=" ++-} id
  toStringI spc (PRead  _ e)         = {-"~" ++-} toStringI spc e
  toStringI spc (PUnit  _)           = "()"
  toStringI spc (PCons  _ hier)      = L.intercalate "." hier
  toStringI spc (PTuple _ es)        = "(" ++ L.intercalate "," (map toString es) ++ ")"
  toStringI spc (PCall  _ p1 p2)     = "(" ++ toString p1 ++ " " ++ toString p2 ++ ")"

-------------------------------------------------------------------------------

instance IString Decl where
  toString decl = toStringI 0 decl

  toStringI spc (DSig _ var tp) = var ++ " :: " ++ toString tp
  toStringI spc (DAtr _ pat wh) = toStringI spc pat ++ " = " ++ toStringI spc wh

-------------------------------------------------------------------------------

instance IString Where where
  toString whe = toStringI 0 whe

  toStringI spc (Where (_,e,[]))   = toStringI spc e
  toStringI spc (Where (_,e,dcls)) = toStringI spc e ++ " where"
                                      ++ (concat $ map (\s -> "\n"++rep (spc+2)++s) (map (toStringI (spc+2)) dcls))
                                      ++ "\n" ++ rep spc ++ ";"

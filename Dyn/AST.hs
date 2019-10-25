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

newtype Decl = Decl (Ann, Patt, Maybe Type, Maybe Where)
  deriving (Eq, Show)

newtype Ifce = Ifce (Ann, (ID_Ifce,ID_Var), [Decl])
  deriving (Eq, Show)

newtype Impl = Impl (Ann, (ID_Ifce,ID_Hier), [Decl])
  deriving (Eq, Show)

type Prog = Where

isEError (EError _ _) = True
isEError _            = False

-------------------------------------------------------------------------------

-- TODO: typeclass

getAnn :: Expr -> Ann
getAnn (EError z _)     = z
getAnn (EVar   z _)     = z
getAnn (EUnit  z)       = z
getAnn (ECons  z _)     = z
getAnn (EData  z _ _)   = z
getAnn (ETuple z _)     = z
getAnn (EFunc  z _ _ _) = z
getAnn (ECall  z _ _)   = z
getAnn (EArg   z)       = z
getAnn (ECase  z _ _)   = z

pattGetAnn :: Patt -> Ann
pattGetAnn (PError z _)     = z
pattGetAnn (PArg   z)       = z
pattGetAnn (PRead  z _)     = z
pattGetAnn (PWrite z _)     = z
pattGetAnn (PUnit  z)       = z
pattGetAnn (PCons  z _)     = z
pattGetAnn (PTuple z _)     = z
pattGetAnn (PCall  z _ _)   = z

declGetAnn :: Decl -> Ann
declGetAnn (Decl (z,_,_,_)) = z

-------------------------------------------------------------------------------

exprToList :: Expr -> [Expr]
exprToList (EUnit  _)    = []
exprToList (ETuple _ es) = es
exprToList e             = [e]

listToExpr :: [Expr] -> Expr
listToExpr []     = EUnit $ error "TODO: z"
listToExpr [x]    = x
listToExpr (x:xs) = ETuple (getAnn x) (x:xs)

-------------------------------------------------------------------------------

listToPatt :: [Patt] -> Patt
listToPatt []     = PUnit $ error "TODO: z"
listToPatt [x]    = x
listToPatt (x:xs) = PTuple (pattGetAnn x) (x:xs)

-------------------------------------------------------------------------------

typeToString :: Type -> String
typeToString (Type (_,ttp,cs)) =
  case cs of
    [] -> ttypeToString ttp
    l  -> ttypeToString ttp ++ " where (" ++ L.intercalate "," (map f l) ++ ")" where
            f (var,[cls]) = var ++ " is " ++ cls
            f (var,clss)  = var ++ " is (" ++ L.intercalate "," clss ++ ")"

ttypeToString :: TType -> String
--ttypeToString TAny            = "?"
ttypeToString TUnit            = "()"
ttypeToString (TVar   id)      = id
ttypeToString (TData  hr)      = L.intercalate "." hr
ttypeToString (TTuple ttps)    = "(" ++ L.intercalate "," (map ttypeToString ttps) ++ ")"
ttypeToString (TFunc  inp out) = "(" ++ ttypeToString inp ++ " -> " ++ ttypeToString out ++ ")"
--ttypeToString (TData ids [x]) = L.intercalate "." ids ++ " of " ++ ttypeToString x
--ttypeToString (TData ids ofs) = L.intercalate "." ids ++ " of " ++ "(" ++ L.intercalate "," (map ttypeToString ofs) ++ ")"

-------------------------------------------------------------------------------

rep spc = replicate spc ' '

exprToString :: Int -> Expr -> String
exprToString spc (EError z msg)         = "(line=" ++ show ln ++ ", col=" ++ show cl ++ ") ERROR : " ++ msg
                                            where (ln,cl) = pos z
exprToString spc (EVar   _ id)          = id
exprToString spc (EUnit  _)             = "()"
exprToString spc (ECons  _ h)           = L.intercalate "." h
exprToString spc (EData  _ h (EUnit _)) = L.intercalate "." h
exprToString spc (EData  _ h st)        = "(" ++ L.intercalate "." h ++ " " ++ exprToString 0 st ++ ")"
exprToString spc (EArg   _)             = "..."
exprToString spc (ETuple _ es)          = "(" ++ L.intercalate "," (map (exprToString 0) es) ++ ")"
exprToString spc (EFunc  _ tp ups bd)   = "func :: " ++ typeToString tp ++ " " ++ upsToString ups ++"->\n" ++ rep (spc+2) ++
                                            whereToString (spc+2) bd ++ "\n" ++ rep spc ++ ";"
                                           where
                                            upsToString []  = ""
                                            upsToString ups = "{" ++ (L.intercalate "," $ map fst ups) ++ "} "
exprToString spc (ECall  _ e1 e2)       = "(" ++ exprToString 0 e1 ++ " " ++ exprToString 0 e2 ++ ")"

exprToString spc (ECase  _ e cases)     =
  "case " ++ exprToString 0 e ++ " of" ++ concat (map f cases) ++ "\n" ++ rep spc ++ ";"
  where
    f :: (Patt,Where) -> String
    f (pat,whe) = "\n" ++ rep (spc+2) ++ pattToString 0 pat ++ " -> " ++ whereToString (spc+2) whe
--exprToString e                    = error $ show e

-------------------------------------------------------------------------------

pattToString :: Int -> Patt -> String
pattToString spc (PArg   _)           = "..."
pattToString spc (PAny   _)           = "_"
pattToString spc (PWrite _ id)        = {-"=" ++-} id
pattToString spc (PRead  _ e)         = {-"~" ++-} exprToString spc e
pattToString spc (PUnit  _)           = "()"
pattToString spc (PCons  _ hier)      = L.intercalate "." hier
pattToString spc (PTuple _ es)        = "(" ++ L.intercalate "," (map (pattToString 0) es) ++ ")"
pattToString spc (PCall  _ p1 p2)     = "(" ++ pattToString 0 p1 ++ " " ++ pattToString 0 p2 ++ ")"

-------------------------------------------------------------------------------

declToString :: Int -> Decl -> String

declToString spc (Decl (_, pat, Just tp, Just w))  = pattToString spc pat ++ " :: " ++ typeToString tp ++ " = " ++ whereToString spc w
declToString spc (Decl (_, pat, Just tp, Nothing)) = pattToString spc pat ++ " :: " ++ typeToString tp ++ ""
declToString spc (Decl (_, pat, Nothing, Just w))  = pattToString spc pat ++ " = " ++ whereToString spc w

-------------------------------------------------------------------------------

whereToString :: Int -> Where -> String

whereToString spc (Where (_,e,[]))   = exprToString spc e
whereToString spc (Where (_,e,dcls)) = exprToString spc e ++ " where" ++
                                        (concat $ map (\s -> "\n"++rep (spc+2)++s) (map (declToString (spc+2)) dcls))
                                        ++ "\n" ++ rep spc ++ ";"

-------------------------------------------------------------------------------

progToString = whereToString 0

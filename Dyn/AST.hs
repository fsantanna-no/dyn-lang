module Dyn.AST where

import Debug.Trace

import qualified Data.List as L

data Ann = Ann { pos :: (Int,Int) }
  deriving (Eq, Show)

az = Ann { pos=(0,0) }

data Type = TUnit
  deriving (Eq, Show)
tz = TUnit

type ID_Var  = String
type ID_Data = String
type ID_Hier = [ID_Data]

data Expr
  = EError Ann String                 -- (msg)        -- error "bug found"
  | EVar   Ann ID_Var                 -- (id)         -- a ; xs
  | EUnit  Ann                        -- ()           -- ()
  | ECons  Ann ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | EData  Ann ID_Hier Expr           -- (ids,struct) -- B.True () ; Int.1 () ; T.Node (T.Leaf(),T.Leaf())
  | ETuple Ann [Expr]                 -- (exprs)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | EFunc  Ann Type Where             -- (type,body)
  | ECall  Ann Expr Expr              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  | EArg   Ann
  | EIf    Ann Expr Patt Where Where  -- (e,p,t,f)    -- if 10 ~> x then t else f
  | ECase  Ann Expr [(Patt,Where)]    -- (exp,[(pat,whe)] -- case x of A->a B->b _->z
  deriving (Eq, Show)

data Patt
  = PError Ann String                 -- (msg)        -- error "bug found"
  | PAny   Ann                        -- ()           -- _
  | PWrite Ann ID_Var                 -- (id)         -- =a ; =xs
  | PRead  Ann Expr                   -- (exp)        -- ~a ; ~xs
  | PUnit  Ann                        -- ()           -- ()
  | PCons  Ann ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | PTuple Ann [Patt]                 -- (patts)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | PCall  Ann Patt Patt              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  deriving (Eq, Show)

newtype Where = Where (Ann, Expr, [Dcl])
  deriving (Eq, Show)

newtype Dcl = Dcl (Ann, Patt, Maybe Type, Maybe Where)
  deriving (Eq, Show)

type Prog = Where

isEError (EError _ _) = True
isEError _            = False

-------------------------------------------------------------------------------

getAnn :: Expr -> Ann
getAnn (EError z _)       = z
getAnn (EVar   z _)       = z
getAnn (EUnit  z)         = z
getAnn (ECons  z _)       = z
getAnn (EData  z _ _)     = z
getAnn (ETuple z _)       = z
getAnn (EFunc  z _ _)     = z
getAnn (ECall  z _ _)     = z
getAnn (EArg   z)         = z
getAnn (EIf    z _ _ _ _) = z
getAnn (ECase  z _ _) = z

-------------------------------------------------------------------------------

rep spc = replicate spc ' '

exprToString :: Int -> Expr -> String
exprToString spc (EError z msg)       = "(line=" ++ show ln ++ ", col=" ++ show cl ++ ") ERROR: " ++ msg
                                          where (ln,cl) = pos z
exprToString spc (EVar   _ id)        = id
exprToString spc (EUnit  _)           = "()"
exprToString spc (ECons  _ hier)      = L.intercalate "." hier
exprToString spc (EData  _ hier st)   = "(" ++ L.intercalate "." hier ++ " " ++ exprToString 0 st ++ ")"
exprToString spc (EArg   _)           = "..."
exprToString spc (ETuple _ es)        = "(" ++ L.intercalate "," (map (exprToString 0) es) ++ ")"
exprToString spc (EFunc  _ TUnit bd)  = "func ()\n" ++ rep (spc+2) ++ whereToString (spc+2) bd
exprToString spc (ECall  _ e1 e2)     = "(" ++ exprToString 0 e1 ++ " " ++ exprToString 0 e2 ++ ")"
exprToString spc (EIf    _ e p t f)   = "if " ++ exprToString 0 e ++ " ~ " ++ pattToString 0 p
                                          ++ " then\n" ++ rep (spc+2) ++ whereToString (spc+2) t
                                          ++ "\n" ++ rep spc ++ "else\n" ++ rep (spc+2) ++ whereToString (spc+2) f
exprToString spc (ECase  _ e cases)   =
  "case " ++ exprToString 0 e ++ " of" ++ concat (map f cases) ++ "\n" ++ rep spc ++ ";"
  where
    f :: (Patt,Where) -> String
    f (pat,whe) = "\n" ++ rep (spc+2) ++ pattToString 0 pat ++ " -> " ++ whereToString (spc+2) whe
--exprToString e                    = error $ show e

-------------------------------------------------------------------------------

pattToString :: Int -> Patt -> String
pattToString spc (PAny   _)           = "_"
pattToString spc (PWrite _ id)        = {-"=" ++-} id
pattToString spc (PRead  _ e)         = {-"~" ++-} exprToString spc e
pattToString spc (PUnit  _)           = "()"
pattToString spc (PCons  _ hier)      = L.intercalate "." hier
pattToString spc (PTuple _ es)        = "(" ++ L.intercalate "," (map (pattToString 0) es) ++ ")"
pattToString spc (PCall  _ p1 p2)     = "(" ++ pattToString 0 p1 ++ " " ++ pattToString 0 p2 ++ ")"

-------------------------------------------------------------------------------

dclToString :: Int -> Dcl -> String

dclToString spc (Dcl (_, pat, Just TUnit, Just w))  = pattToString spc pat ++ " :: () = " ++ whereToString spc w
dclToString spc (Dcl (_, pat, Just TUnit, Nothing)) = pattToString spc pat ++ " :: ()"
dclToString spc (Dcl (_, pat, Nothing,    Just w))  = pattToString spc pat ++ " = " ++ whereToString spc w

-------------------------------------------------------------------------------

whereToString :: Int -> Where -> String

whereToString spc (Where (_,e,[]))   = exprToString 0 e
whereToString spc (Where (_,e,dcls)) = exprToString 0 e ++ " where" ++
                                        (concat $ map (\s -> "\n"++rep (spc+2)++s) (map (dclToString (spc+2)) dcls))

-------------------------------------------------------------------------------

progToString = whereToString 0

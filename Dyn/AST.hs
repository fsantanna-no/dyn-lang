module Dyn.AST where

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
  = EError Ann
  | EAny   Ann                        -- ()           -- _
  | EVar   Ann ID_Var                 -- (id)         -- a ; xs
  | EUnit  Ann                        -- ()           -- ()
  | ECons  Ann ID_Hier                -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | EData  Ann ID_Hier Expr           -- (ids,struct) -- B.True () ; Int.1 () ; T.Node (T.Leaf(),T.Leaf())
  | ETuple Ann [Expr]                 -- (exprs)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | EFunc  Ann Type Where             -- (type,body)
  | ECall  Ann Expr Expr              -- (func,arg)   -- f a ; f(a) ; f(1,2)
  | EArg   Ann
  | EIf    Ann Expr Expr Where Where  -- (e,p,t,f)    -- if 10 ~> x then t else f
  deriving (Eq, Show)

newtype Where = Where (Ann, Expr, [Dcl])
  deriving (Eq, Show)

newtype Dcl = Dcl (Ann, Expr, Maybe Type, Maybe Where)
  deriving (Eq, Show)

type Prog = Where

isEError (EError _) = True
isEError _          = False

-------------------------------------------------------------------------------

getAnn :: Expr -> Ann
getAnn (EError z)         = z
getAnn (EAny   z)         = z
getAnn (EVar   z _)       = z
getAnn (EUnit  z)         = z
getAnn (ECons  z _)       = z
getAnn (EData  z _ _)     = z
getAnn (ETuple z _)       = z
getAnn (EFunc  z _ _)     = z
getAnn (ECall  z _ _)     = z
getAnn (EArg   z)         = z
getAnn (EIf    z _ _ _ _) = z

-------------------------------------------------------------------------------

rep spc = replicate spc ' '

exprToString :: Int -> Expr -> String
exprToString spc (EError _)           = "error"
exprToString spc (EAny   _)           = "_"
exprToString spc (EVar   _ id)        = id
exprToString spc (EUnit  _)           = "()"
exprToString spc (ECons  _ hier)      = L.intercalate "." hier
exprToString spc (EData  _ hier st)   = "(" ++ L.intercalate "." hier ++ " " ++ exprToString 0 st ++ ")"
exprToString spc (EArg   _)           = "..."
exprToString spc (ETuple _ es)        = "(" ++ L.intercalate "," (map (exprToString 0) es) ++ ")"
exprToString spc (EFunc  _ TUnit bd)  = "func ()\n" ++ rep (spc+2) ++ whereToString (spc+2) bd
exprToString spc (ECall  _ e1 e2)     = "(" ++ exprToString 0 e1 ++ " " ++ exprToString 0 e2 ++ ")"
exprToString spc (EIf    _ p e t f)   = "if " ++ exprToString 0 p ++ " ~ " ++ exprToString 0 e
                                          ++ " then\n" ++ rep (spc+2) ++ whereToString (spc+2) t
                                          ++ "\n" ++ rep spc ++ "else\n" ++ rep (spc+2) ++ whereToString (spc+2) f
--exprToString e                    = error $ show e

-------------------------------------------------------------------------------

dclToString :: Int -> Dcl -> String

dclToString spc (Dcl (_, pat, Just TUnit, Just w))  = rep spc ++ exprToString spc pat ++ " :: () = " ++ whereToString spc w
dclToString spc (Dcl (_, pat, Just TUnit, Nothing)) = rep spc ++ exprToString spc pat ++ " :: ()"
dclToString spc (Dcl (_, pat, Nothing,    Just w))  = rep spc ++ exprToString spc pat ++ " = " ++ whereToString spc w

-------------------------------------------------------------------------------

whereToString :: Int -> Where -> String

whereToString spc (Where (_,e,[]))   = exprToString 0 e
whereToString spc (Where (_,e,dcls)) = exprToString 0 e ++ " where\n" ++
                                        L.intercalate "\n" (map (dclToString (spc+2)) dcls)

-------------------------------------------------------------------------------

progToString = whereToString 0

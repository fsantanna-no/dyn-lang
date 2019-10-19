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

exprToString :: Expr -> String
exprToString (EError _)           = "error"
exprToString (EAny   _)           = "_"
exprToString (EVar   _ id)        = id
exprToString (EUnit  _)           = "()"
exprToString (ECons  _ hier)      = L.intercalate "." hier
exprToString (EData  _ hier st)   = "(" ++ L.intercalate "." hier ++ " " ++ exprToString st ++ ")"
exprToString (EArg   _)           = "..."
exprToString (ETuple _ es)        = "(" ++ L.intercalate "," (map exprToString es) ++ ")"
exprToString (EFunc  _ TUnit bd)  = "func () " ++ whereToString 0 bd
exprToString (ECall  _ e1 e2)     = "(" ++ exprToString e1 ++ " " ++ exprToString e2 ++ ")"
exprToString (EIf    _ p e t f)   = "if " ++ exprToString p ++ " ~ " ++ exprToString e
                                      ++ " then " ++ whereToString 0 t
                                      ++ " else " ++ whereToString 0 f
--exprToString e                    = error $ show e

-------------------------------------------------------------------------------

dclToString :: Int -> Dcl -> String

dclToString spc (Dcl (_, pat, Just TUnit, Just w))  = replicate spc ' ' ++ exprToString pat ++ " :: () = " ++ whereToString spc w
dclToString spc (Dcl (_, pat, Just TUnit, Nothing)) = replicate spc ' ' ++ exprToString pat ++ " :: ()"
dclToString spc (Dcl (_, pat, Nothing,    Just w))  = replicate spc ' ' ++ exprToString pat ++ " = " ++ whereToString spc w

-------------------------------------------------------------------------------

whereToString :: Int -> Where -> String

whereToString spc (Where (_,e,[]))   = exprToString e
whereToString spc (Where (_,e,dcls)) = exprToString e ++ " where\n" ++
                                        L.intercalate "\n" (map (dclToString (spc+2)) dcls)

-------------------------------------------------------------------------------

progToString = whereToString 0

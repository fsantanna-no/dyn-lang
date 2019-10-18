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
  | EAny   Ann                      -- ()           -- _
  | EVar   Ann ID_Var               -- (id)         -- a ; xs
  | EUnit  Ann                      -- ()           -- ()
  | ECons  Ann ID_Hier              -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | ETuple Ann [Expr]               -- (exprs)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | EFunc  Ann Type Expr            -- (type,body)
  | ECall  Ann Expr Expr            -- (func,arg)   -- f a ; f(a) ; f(1,2)
  | EArg   Ann
  | EIf    Ann Expr Expr Expr Expr  -- (e,p,t,f)    -- if 10 ~> x then t else f
  deriving (Eq, Show)

newtype Where = Where (Ann, Expr, [Dcl])
  deriving (Eq, Show)

newtype Dcl = Dcl (Ann, Expr, Maybe Type, Maybe Where)
  deriving (Eq, Show)

type Prog = Where

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

exprToString :: Expr -> String
exprToString (EError _)           = "error"
exprToString (EAny   _)           = "_"
exprToString (EVar   _ id)        = id
exprToString (EUnit  _)           = "()"
exprToString (ECons  _ ["Int",n]) = n
exprToString (ECons  _ hier)      = L.intercalate "." hier
exprToString (EArg   _)           = "..."
exprToString (ETuple _ es)        = "(" ++ L.intercalate "," (map exprToString es) ++ ")"
exprToString (EFunc  _ TUnit e)   = "func () " ++ exprToString e
exprToString (ECall  _ e1 e2)     = "(" ++ exprToString e1 ++ " " ++ exprToString e2 ++ ")"
exprToString (EIf    _ p e t f)   = "if " ++ exprToString p ++ " ~ " ++ exprToString e
                                      ++ " then " ++ exprToString t
                                      ++ " else " ++ exprToString f
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

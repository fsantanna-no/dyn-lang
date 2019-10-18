module Dyn.AST where

import qualified Data.List as L

data Ann = Ann { pos :: (Int,Int) }
  deriving (Eq, Show)

az = Ann { pos=(0,0) }

type Type = ()

type ID_Var  = String
type ID_Data = String
type ID_Hier = [ID_Data]

data Expr
  = EError Ann Int
  | EVar   Ann ID_Var               -- (id)         -- a ; xs
  | EUnit  Ann                      -- ()           -- ()
  | ECons  Ann ID_Hier              -- (ids)        -- Bool.True ; Int.1 ; Tree.Node
  | ETuple Ann [Expr]               -- (exprs)      -- (1,2) ; ((1,2),3) ; ((),()) // (len >= 2)
  | EFunc  Ann Type Expr            -- (type,body)
  | ECall  Ann Expr Expr            -- (func,arg)   -- f a ; f(a) ; f(1,2)
  | EArg   Ann
  | EIf    Ann Expr Expr Expr Expr  -- (p,e,t,f)    -- if 10 matches x then t else f
  deriving (Eq, Show)

newtype Expr' = Expr' (Expr,[Dcl])
newtype Dcl   = Dcl   (ID_Var, Type, Expr')

type Prog  = Expr'

exprToString :: Int -> Expr -> String
exprToString spc (EError _ n)         = "error " ++ show n
exprToString spc (EVar   _ id)        = id
exprToString spc (EUnit  _)           = "()"
exprToString spc (ECons  _ ["Int",n]) = n
--exprToString spc (ECons  _ hier)      = hier2str hier
exprToString spc (EArg   _)           = "..."
exprToString spc (ETuple _ es)        = "(" ++ L.intercalate "," (map (exprToString spc) es) ++ ")"
--exprToString spc (EFunc  _ _ e)       = "func" ++ "\n" ++ show_stmt (spc+4) p
exprToString spc (ECall  _ e1 e2)     = exprToString spc e1 ++ " " ++ exprToString spc e2
exprToString spc e                    = error $ show e

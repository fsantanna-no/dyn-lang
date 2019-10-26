module Dyn.Classes where

import Dyn.AST

-------------------------------------------------------------------------------

instance IAnn Expr where
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

instance IAnn Patt where
  getAnn (PError z _)     = z
  getAnn (PArg   z)       = z
  getAnn (PRead  z _)     = z
  getAnn (PWrite z _)     = z
  getAnn (PUnit  z)       = z
  getAnn (PCons  z _)     = z
  getAnn (PTuple z _)     = z
  getAnn (PCall  z _ _)   = z

instance IAnn Decl where
  getAnn (DSig z _ _) = z
  getAnn (DAtr z _ _) = z

-------------------------------------------------------------------------------

instance IList Expr where
  toList (EUnit  _)    = []
  toList (ETuple _ es) = es
  toList e             = [e]

  fromList []     = EUnit $ error "TODO: z"
  fromList [x]    = x
  fromList (x:xs) = ETuple (getAnn x) (x:xs)

instance IList TType where
  toList TUnit         = []
  toList (TTuple ttps) = ttps
  toList ttp           = [ttp]

  fromList x = error "TODO"

instance IList Patt where
  toList x = error "TODO"

  fromList []     = PUnit $ error "TODO: z"
  fromList [x]    = x
  fromList (x:xs) = PTuple (getAnn x) (x:xs)

-------------------------------------------------------------------------------



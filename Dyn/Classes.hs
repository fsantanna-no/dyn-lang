module Dyn.Classes where

import Data.Bool (bool)
import Data.List (intercalate)

import Dyn.AST

rep spc = replicate spc ' '

-------------------------------------------------------------------------------
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
-------------------------------------------------------------------------------

instance IList Expr where
  toList (EUnit  _)    = []
  toList (ETuple _ es) = es
  toList e             = [e]

  fromList []     = EUnit az -- TODO: az{pos=?}
  fromList [x]    = x
  fromList (x:xs) = ETuple (getAnn x) (x:xs)

instance IList TType where
  toList TUnit         = []
  toList (TTuple ttps) = ttps
  toList ttp           = [ttp]

  fromList x = error "TODO"

instance IList Patt where
  toList x = error "TODO"

  fromList []     = PUnit $ error "TODO: fromList"
  fromList [x]    = x
  fromList (x:xs) = PTuple (getAnn x) (x:xs)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

instance IString Type where
  toString (Type (_,ttp,cs)) =
    case cs of
      [] -> toString ttp
      l  -> toString ttp ++ " where (" ++ intercalate "," (map f l) ++ ")" where
              f (var,[cls]) = var ++ " is " ++ cls
              f (var,clss)  = var ++ " is (" ++ intercalate "," clss ++ ")"
  toStringI _ = error "TODO"


-------------------------------------------------------------------------------

instance IString TType where
  toString TAny             = "?"
  toString TUnit            = "()"
  toString (TVar   id)      = id
  toString (TData  hr)      = intercalate "." hr
  toString (TTuple ttps)    = "(" ++ intercalate "," (map toString ttps) ++ ")"
  toString (TFunc  inp out) = "(" ++ toString inp ++ " -> " ++ toString out ++ ")"
  --toString (TData ids [x]) = intercalate "." ids ++ " of " ++ toString x
  --toString (TData ids ofs) = intercalate "." ids ++ " of " ++ "(" ++ intercalate "," (map toString ofs) ++ ")"

  toStringI _ = error "TODO"

-------------------------------------------------------------------------------

instance IString Expr where
  toString expr = toStringI 0 expr

  toStringI spc (EError z msg)         = "(line=" ++ show ln ++ ", col=" ++ show cl ++ ") ERROR : " ++ msg
                                              where (ln,cl) = pos z
  toStringI spc (EVar   _ id)          = id
  toStringI spc (EUnit  _)             = "()"
  toStringI spc (ECons  _ h)           = intercalate "." h
  toStringI spc (EData  _ h (EUnit _)) = intercalate "." h
  toStringI spc (EData  _ h st)        = "(" ++ intercalate "." h ++ " " ++ toString st ++ ")"
  toStringI spc (EArg   _)             = "..."
  toStringI spc (ETuple _ es)          = "(" ++ intercalate "," (map toString es) ++ ")"
  toStringI spc (EFunc  _ tp ups bd)   = "func :: " ++ toString tp ++ " " ++ upsToString ups ++"->\n" ++ rep (spc+2) ++
                                              toStringI (spc+2) bd ++ "\n" ++ rep spc ++ ";"
                                             where
                                              upsToString []  = ""
                                              upsToString ups = "{" ++ (intercalate "," $ map fst ups) ++ "} "
  toStringI spc (ECall  _ e1 e2)       = "(" ++ toString e1 ++ " " ++ toString e2 ++ ")"

  toStringI spc (ECase  _ e cases)     =
    "case " ++ toString e ++ " of" ++ concat (map f cases) ++ "\n" ++ rep spc ++ ";"
    where
      f :: (Patt,Where) -> String
      f (pat,whe) = "\n" ++ rep (spc+2) ++ pattToString True pat ++ " -> " ++ toStringI (spc+2) whe
  --toStringI e                    = error $ show e

-------------------------------------------------------------------------------

pattToString :: Bool -> Patt -> String
pattToString _ (PArg   _)           = "..."
pattToString _ (PAny   _)           = "_"
pattToString s (PWrite _ id)        = (bool "" "=" s) ++ id
pattToString s (PRead  _ e)         = (bool "" "~" s) ++ toString e
pattToString _ (PUnit  _)           = "()"
pattToString _ (PCons  _ hier)      = intercalate "." hier
pattToString s (PTuple _ es)        = "(" ++ intercalate "," (map (pattToString s) es) ++ ")"
pattToString s (PCall  _ p1 p2)     = "(" ++ pattToString s p1 ++ " " ++ pattToString s p2 ++ ")"

-------------------------------------------------------------------------------

instance IString Decl where
  toString decl = toStringI 0 decl

  toStringI spc (DSig _ var tp) = var ++ " :: " ++ toString tp
  toStringI spc (DAtr _ pat wh) = pattToString False pat ++ " = " ++ toStringI spc wh

-------------------------------------------------------------------------------

instance IString Where where
  toString whe = toStringI 0 whe

  toStringI spc (Where (_,e,[]))   = toStringI spc e
  toStringI spc (Where (_,e,dcls)) = toStringI spc e ++ " where"
                                      ++ (concat $ map (\s -> "\n"++rep (spc+2)++s) (map (toStringI (spc+2)) dcls))
                                      ++ "\n" ++ rep spc ++ ";"

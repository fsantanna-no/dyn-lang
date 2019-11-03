module Dyn.Classes where

import Data.Bool (bool)
import Data.List (intercalate)

import Dyn.AST

rep spc = replicate spc ' '

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

instance IPos Expr where
  getPos (EError z _)     = z
  getPos (EVar   z _)     = z
  getPos (EUnit  z)       = z
  getPos (ECons  z _)     = z
  getPos (EData  z _ _)   = z
  getPos (ETuple z _)     = z
  getPos (EFunc  z _ _ _ _) = z
  getPos (ECall  z _ _)   = z
  getPos (EArg   z)       = z
  getPos (ECase  z _ _)   = z

instance IPos Patt where
  getPos (PError z _)     = z
  getPos (PArg   z)       = z
  getPos (PRead  z _)     = z
  getPos (PWrite z _)     = z
  getPos (PUnit  z)       = z
  getPos (PCons  z _)     = z
  getPos (PTuple z _)     = z
  getPos (PCall  z _ _)   = z

instance IPos Decl where
  getPos (DSig z _ _ _) = z
  getPos (DAtr z _ _)   = z

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

instance IList Expr where
  toList (EUnit  _)    = []
  toList (ETuple _ es) = es
  toList e             = [e]

  fromList []     = EUnit pz -- TODO: pz{pos=?}
  fromList [x]    = x
  fromList (x:xs) = ETuple (getPos x) (x:xs)

instance IList Type where
  toList TUnit        = []
  toList (TTuple tps) = tps
  toList tp           = [tp]

  fromList x = error "TODO"

instance IList Patt where
  toList x = error "TODO"

  fromList []     = PUnit $ error "TODO: fromList"
  fromList [x]    = x
  fromList (x:xs) = PTuple (getPos x) (x:xs)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

instance IString Type where
  toString TAny             = "?"
  toString TUnit            = "()"
  toString (TVar   id)      = id
  toString (TData  hr)      = intercalate "." hr
  toString (TTuple ttps)    = "(" ++ intercalate "," (map toString ttps) ++ ")"
  toString (TFunc  inp out) = "(" ++ toString inp ++ " -> " ++ toString out ++ ")"
  --toString (TData ids [x]) = intercalate "." ids ++ " of " ++ toString x
  --toString (TData ids ofs) = intercalate "." ids ++ " of " ++ "(" ++ intercalate "," (map toString ofs) ++ ")"

  toStringI _ _ = error "TODO"

instance IString Ctrs where
  toString (Ctrs []) = ""
  toString (Ctrs cs) = " where (" ++ f cs ++ ")" where
    f [ifc] = "a" ++ " is " ++ ifc
    f ifcs  = "a" ++ " is (" ++ intercalate "," ifcs ++ ")"
{-
  toString (Ctrs cs) = " where (" ++ intercalate "," (map f cs) ++ ")" where
    f (var,[cls]) = var ++ " is " ++ cls
    f (var,clss)  = var ++ " is (" ++ intercalate "," clss ++ ")"
-}

  toStringI _ _ = error "TODO"

 -------------------------------------------------------------------------------

instance IString Expr where
  toString expr = toStringI 0 expr

  toStringI spc (EError z msg)         = "(line=" ++ show ln ++ ", col=" ++ show cl ++ ") ERROR : " ++ msg
                                              where (ln,cl) = z
  toStringI spc (EVar   _ id)          = id
  toStringI spc (EUnit  _)             = "()"
  toStringI spc (ECons  _ h)           = intercalate "." h
  toStringI spc (EArg   _)             = "..."
  toStringI spc (ETuple _ es)          = "(" ++ intercalate "," (map toString es) ++ ")"
  toStringI spc (EFunc  _ cs tp ups bd)  = "func :: " ++ toString tp ++ " " ++ upsToString ups ++"->\n" ++ rep (spc+2) ++
                                              toStringI (spc+2) bd ++ "\n" ++ rep spc ++ ";"
                                             where
                                              upsToString []  = ""
                                              upsToString ups = "{" ++ (intercalate "," $ map fst ups) ++ "} "
  toStringI spc (ECall  _ e1 e2)       = "(" ++ toString e1 ++ " " ++ toString e2 ++ ")"

  toStringI spc (ECase  _ e cases)     =
    "case " ++ toString e ++ " of" ++ concat (map f cases) ++ "\n" ++ rep spc ++ ";"
    where
      f :: (Patt,ExpWhere) -> String
      f (pat,whe) = "\n" ++ rep (spc+2) ++ pattToString True pat ++ " -> " ++ toStringI (spc+2) whe
  toStringI spc (EData  _ h (EUnit _)) = intercalate "." h
  toStringI spc (EData  _ h st)        = "(" ++ intercalate "." h ++ " " ++ toString st ++ ")"
  toStringI spc (EType  _ ttp)         = "(Type " ++ toString ttp ++ ")"
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

  toStringI spc (DSig _ var cs tp) = var ++ " :: " ++ toString tp -- ++ toString cs
  toStringI spc (DAtr _ pat wh)    = pattToString False pat ++ " = " ++ toStringI spc wh

-------------------------------------------------------------------------------

instance IString ExpWhere where
  toString whe = toStringI 0 whe

  toStringI spc (ExpWhere (_,e,[]))   = toStringI spc e
  toStringI spc (ExpWhere (_,e,dcls)) = toStringI spc e ++ " where"
                                        ++ (concat $ map (\s -> "\n"++rep (spc+2)++s) (map (toStringI (spc+2)) dcls))
                                        ++ "\n" ++ rep spc ++ ";"

-------------------------------------------------------------------------------

instance IString Prog where
  toString (Prog glbs) = concatMap f glbs where
                          f (GDecl decl) = toString decl ++ "\n"
                          f (GIfce ifce) = error "TODO"
                          f (GImpl impl) = error "TODO"
  toStringI _ _ = error "TODO"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

instance IType Expr where
  toType _  (EArg   _)     = TAny
  toType ds (EVar   _ id)  = snd $ dsigsFind ds id
  toType _  (ECons  _ hr)  = TData hr
  toType ds (ETuple _ es)  = TTuple $ map (toType ds) es
  toType ds (ECall  _ f _) = case toType ds f of
                                TAny        -> TAny
                                TFunc _ out -> out
                                TData hr    -> TData hr
  toType _  e = error $ "toType: " ++ toString e

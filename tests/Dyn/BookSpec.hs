{-# LANGUAGE QuasiQuotes #-}

module Dyn.BookSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Analyse
import Dyn.Prelude

main :: IO ()
main = hspec spec

-------------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "PRE" $ do
    it "Nat +" $
      evalString ("main = add (Nat.Zero, Nat.Succ Nat.Zero)\n" ++ nat)
        `shouldBe` "(Nat.Succ Nat.Zero)"
    it "Nat -" $
      evalString ("main = sub (Nat.Succ Nat.Zero, Nat.Zero)\n" ++ nat)
        `shouldBe` "(Nat.Succ Nat.Zero)"
    it "Nat *" $
      evalString ("main = mul (two,three)\n" ++ nat)
        `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))))"
    it "Nat %" $
      evalString ("main = rem (two,three)\n" ++ prelude)
        `shouldBe` "(Nat.Succ (Nat.Succ Nat.Zero))"

-------------------------------------------------------------------------------

    -- TODO-3-20: square : Float -> Float

  describe "Chapter 1 - Fundamental Concepts:" $ do           -- pg 1

-------------------------------------------------------------------------------

    describe "Chapter 1.1 - Sessions and Scripts:" $ do       -- pg 1

      it "square" $
        evalString ([r|
main = square two
square =
  func -> let x = ... in
    mul (x,x)
  ;;
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))"

      it "lt" $
        evalString ([r|
main = (nlte (three,two), nlte (three,three))
|] ++ nat)
          `shouldBe` "(Bool.False,Bool.True)"

      it "smaller" $                  -- pg 2
        evalString ([r|
main = add (smaller (ten,five) , smaller (one,four))
smaller =
  func ->
    let (x,y) = ... in
      case nlte (x,y) of
        Bool.True  -> x
        Bool.False -> y
      ;case
    ;let
  ;func
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))))"


      it "square/smaller" $           -- pg 3
        evalString ([r|
main = square (smaller (four, two))
square =
  func -> let x=... in
    mul (x,x)
  ;;
smaller =
  func ->
    case nlte (x,y) of
      Bool.True  -> x
      Bool.False -> y
    ;case
      where
        (x,y) = ...
      ;where
  ;func
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))"

      -- TODO-3-delta

-------------------------------------------------------------------------------

    describe "Chapter 1.2 - Evaluation:" $ do                 -- pg 4

      it "three" $            -- pg 5
        evalString ([r|
main = fthree ten
fthree =
  func ->
    three
  ;
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero)))"

{-
      it "infinity" $         -- pg 5
        (run True $
          unlines [
            "func infinity () : (() -> Nat) do",
            "   return (infinity()) + 1",
            "end",
            "return infinity ()"
           ])
        `shouldBe` Right (EError (-1))

      it "three/infinity" $   -- pg 5
        (run True $
          unlines [
            "func three (x) : (Nat -> Nat) do",
            "   return 3",
            "end",
            "func infinity () : (() -> Nat) do",
            "   return (infinity()) + 1",
            "end",
            "return three (infinity ())"
           ])
        `shouldBe` Right (EError (-1))
-}

-------------------------------------------------------------------------------

    describe "Chapter 1.3 - Values:" $ do                     -- pg 7

      it "multiply 2 3" $     -- pg 9
        evalString ([r|
main = multiply (two,three)
multiply =
  func ->
    case ... of
      (~zero, _)  -> zero
      (=x,    =y) -> mul (x,y)
    ;
  ;
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))))"

{-
      it "multiply 3 infinity" $  -- pg 9
        (run True $
          unlines [
            "func multiply (x,y) : ((Nat,Nat) -> Nat) do",
            "   if x == 0 then",
            "     return 0",
            "   else",
            "     return x * y",
            "   end",
            "end",
            "func infinity () : (() -> Nat) do",
            "   return (infinity()) + 1",
            "end",
            "return multiply (3,infinity())"
           ])
          `shouldBe` Right (EError (-1))
-}

-------------------------------------------------------------------------------

    describe "Chapter 1.4 - Functions:" $ do                  -- pg 9

      describe "Chapter 1.4.2 - Currying:" $ do               -- pg 11

        it "smallerc" $            -- pg 11
          evalString ([r|
main = (smallerc two) four
smallerc =
  func :: (Nat -> (Nat -> Nat)) ->
    func :: (Nat -> Nat) {x} ->
      lt (x',y) where
        x' = x
        y  = ...
      ;
    ; where
      x = ...
    ;
  ;
|] ++ prelude)
          `shouldBe` "Bool.True"


        it "twice" $            -- pg 12
          evalString ([r|
main = twice (square,two)
square = func -> mul (...,...) ;
twice = func ->
  case ... of
    (=f,=x) -> f (f x)
  ;
;
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))))))))))))))"

        it "twicec" $            -- pg 12
          evalString ([r|
main   = matches ((twicec square) two, mul(four,four))
square = func -> mul (...,...);
twicec = func ->
  func {f} ->
    f (f ...)
  ; where
    f = ...
  ;
;
|] ++ nat ++ std)
          `shouldBe` "Bool.True"

        it "quad" $            -- pg 12
          evalString ([r|
main   = matches (quad two, mul (four,four))
quad   = twicec square
square = func -> mul (...,...);
twicec = func ->
  func {f} ->
    f (f ...)
  ; where
    f = ...
  ;
;
|] ++ prelude)
          `shouldBe` "Bool.True"

        it "curry" $            -- pg 13
          evalString ([r|
main   = matches ((twicec square) two, mul(four,four))
square = func -> mul (...,...);
twicec = curry twice
twice  = func ->
  case ... of
    (=f,=x) -> f (f x)
  ;
;
curry  = func ->
  func {f} ->
    func {f,x} ->
      f (x,...)
    ; where
      x = ...
    ;
  ; where
    f = ...
  ;
;
|] ++ prelude)
          `shouldBe` "Bool.True"

      describe "Chapter 1.4.3 - Operators:" $ do               -- pg 13

{-
      -- TODO: operators
        it "+" $                -- pg 13
          (run True $
            unlines [
              "return 1 + (+ (2,3))"
             ])
          `shouldBe` Right (EData ["Nat","6"] EUnit)
-}

        it "uncurry" $            -- pg 11
          evalString ([r|
main = (uncurry smallerc) (two,ten)
smallerc = func ->
  func :: (Nat->Nat) {x} ->
    lt (x',y) where
      x' = x
      y  = ...
    ;
  ; where
    x = ...
  ;
;
uncurry = func ->
  func {f} ->
    (f i) j where
      (i,j) = ...
    ;
  ; where
    f = ...
  ;
;
|] ++ prelude)
          `shouldBe` "Bool.True"

      describe "Chapter 1.4.7 - Composition:" $ do               -- pg 15

        it "compose" $         -- pg 15
          evalString ([r|
main    = matches (quad two, mul (four,four))
quad    = compose (square,square)
square  = func -> mul (...,...);
compose = func ->
  func {f,g} ->
    f (g ...)
  ; where
    (f,g) = ...
  ;
;
|] ++ prelude)
          `shouldBe` "Bool.True"

-------------------------------------------------------------------------------

    describe "Chapter 1.5 - Definitions:" $ do                -- pg 17

{-
      -- TODO: negative numbers
      it "signum" $           -- pg 18
        (run True $
          unlines [
            "func signum (x) : (Nat->Nat) do",
            "   if x < 0 then",
            "     return  -1",
            "   else/if x == 0 then",
            "     return 0",
            "   else",
            "     return 1",
            "   end",
            "end",
            "return (signum 1) + ((signum (-10)) + (signum (10-10)))"
           ])
        `shouldBe` Right (EData ["Nat","0"] EUnit)
-}

      it "fact" $             -- pg 19
        evalString ([r|
main = fact three
fact =
  func ->
    case ... of
      Nat.Zero -> one
      =n       -> mul (n, fact (dec n))
    ;
  ;
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))))"

{-
      -- TODO: negative numbers
      it "fact - error" $     -- pg 20
        (run True $
          unlines [
            "func fact (n) : (Nat->Nat) do",
            "   if n < 0 then",
            "     error 1",
            "   else/if n == 0 then",
            "     return 1",
            "   else",
            "     return n * (fact (n-1))",
            "   end",
            "end",
            "return fact (-5)"
           ])
        `shouldBe` Right (EError 1)
-}

      it "locals" $           -- pg 20
        evalString ([r|
main = f (zero,one)
f =
  func ->
    mul (add(a,one), add(a,two)) where
      a = add (x,y)
      (x,y) = ...
    ;
  ;
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))))"

      it "locals" $           -- pg 21
        evalString ([r|
main = f (zero,one)
f =
  func ->
    mul (add(a,one), add(b,two)) where
      a = add (x,y)
      b = mul (x,y)
      (x,y) = ...
    ;
  ;
|] ++ nat)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero))))"

-------------------------------------------------------------------------------

    --describe "Chapter 1.6 - Types:" $ do                      -- pg 21

-------------------------------------------------------------------------------

    --describe "Chapter 1.7 - Specifications:" $ do             -- pg 25

-------------------------------------------------------------------------------

  describe "Chapter 2 - Simple Datatypes:" $ do               -- pg 29

-------------------------------------------------------------------------------

    describe "Chapter 2.1 - Booleans:" $ do                   -- pg 29

{-
      -- TODO: typesys
      it "data" $             -- pg 29
        (run True $
          unlines [
            "data Bool_",
            "data Bool_.False_",
            "data Bool_.True_",
            "return Bool_.True_"
          ])
        `shouldBe` Right (EData ["Bool_","True_"] EUnit)
-}

      it "not" $              -- pg 30
        evalString ([r|
main = not Bool.False
not = func ->
  case ... of
    Bool.False -> Bool.True
    Bool.True  -> Bool.False
  ;
;
|])
          `shouldBe` "Bool.True"

      it "and-1" $            -- pg 30
        evalString ([r|
main = and (Bool.True, Bool.False)
and = func ->
  case ... of
    (Bool.False, _) -> Bool.False
    (_, Bool.False) -> Bool.False
    _               -> Bool.True
  ;
;
|])
          `shouldBe` "Bool.False"

      it "and-2" $            -- pg 30
        evalString ([r|
main = and (Bool.True, Bool.True)
|] ++ bool)
          `shouldBe` "Bool.True"

      it "or-1" $               -- pg 30
        evalString ([r|
main = or (Bool.True, Bool.False)
or = func ->
  case ... of
    (Bool.True, _)  -> Bool.True
    (_,         =y) -> y
  ;
;
|])
          `shouldBe` "Bool.True"

      it "or-2" $               -- pg 30
        evalString ([r|
main = or (Bool.False, Bool.False)
or = func ->
  case ... of
    (Bool.True, _)  -> Bool.True
    (_,         =y) -> y
  ;
;
|])
          `shouldBe` "Bool.False"

      it "eq, neq" $         -- pg 31
        evalString ([r|
main = neq (eq (Bool.True,Bool.True), Bool.False)
eq = func ->
  or (and (x,y), (and (not x, not y))) where
    (x,y) = ...
  ;
;
neq = func ->
  not (eq (x,y)) where
    (x,y) = ...
  ;
;
|] ++ bool)
          `shouldBe` "Bool.True"

      it "@<=" $
        evalString ([r|
main = lte (Xx.Aa,Xx.Bb)
--data Xx
--data Xx.Aa
--data Xx.Bb
|] ++ iord ++ ieq)
        `shouldBe` "(line=2, col=8) ERROR : unassigned variable 'dIEqXx'"

      it "leap years" $         -- pg 33
        evalString ([r|
main = and (not (leapyear y1979), and (leapyear y1980, and (not (leapyear hundred), leapyear (mul (four,hundred)))))
leapyear :: (Nat->Bool) = func ->
  case rem (y,hundred) of
    Nat.Zero -> eq(rem (y, mul(four,hundred)), Nat.Zero)
    _        -> eq(rem (y, four),              Nat.Zero)
  ; where
    y = ...
  ;
;
y1979 = sub (y1980, one)
y1980 = add (thousand, add (mul(five,hundred), mul(eight,ten)))
|] ++ prelude)
        `shouldBe` "Bool.True"

      it "analyse triangles" $         -- pg 33
        evalString ([r|
main = (analyse (ten, twenty, mul(ten,three)),
        analyse (ten, twenty, add(twenty,five)),
        analyse (ten, twenty, twenty),
        analyse (ten, ten,    ten))
twenty = add (ten,ten)
analyse = func ->
  case nlte (add (x,y), z) of
    Bool.True -> Tri.Fail
    _ -> case (x,y,z) of
      (~z,_,_) -> Tri.Equi
      (~y,_,_) -> Tri.Isos
      (_,_,~y) -> Tri.Isos
      _        -> Tri.Scal
    ;
  ; where
    (x,y,z) = ...
  ;
;
|] ++ nat)
          `shouldBe` "(Tri.Fail,Tri.Scal,Tri.Isos,Tri.Equi)"

      it "implication" $         -- pg 34
        evalString ([r|
main = (impl (Bool.False,Bool.True),
        impl (Bool.True, Bool.True),
        impl (Bool.False,Bool.False),
        not (impl (Bool.True,Bool.False)))

impl = func ->
  or (not x, y) where
    (x,y) = ...
  ;
;
|] ++ bool)
          `shouldBe` "(Bool.True,Bool.True,Bool.True,Bool.True)"

      it "analyse triangles" $         -- pg 35
        evalString ([r|
main = ( analyse2 (add(twenty,ten),twenty,ten)  ,
         analyse2 (ten,add(twenty,five),twenty) ,
         analyse2 (ten,twenty,twenty)           ,
         analyse2 (ten,ten,ten)                 )

twenty = mul (two,ten)

--data Triangle
--data Triangle.Failure
--data Triangle.Isosceles
--data Triangle.Equilateral
--data Triangle.Scalene

analyse2 :: ((Nat,Nat,Nat) -> Triangle) = func ->
  case (lte(x,y),  lte(x,z),  lte(y,x),  lte(y,z) ) of
       (Bool.True, Bool.True, _,         Bool.True)  -> analyse (x,y,z)
       (Bool.True, Bool.True, _,         Bool.False) -> analyse (x,z,y)
       (_,         Bool.True, Bool.True, Bool.True)  -> analyse (y,x,z)
       (_,         Bool.False,Bool.True, Bool.True)  -> analyse (y,z,x)
       (Bool.True, _,         _,         _)          -> analyse (z,x,y)
       _                                             -> analyse (z,y,x)
  ; where
    (x,y,z) = ...
  ;
;

analyse = func :: ((Nat,Nat,Nat) -> Triangle) ->
  case lte (add (x,y), z) of
    Bool.True  -> Triangle.Failure
    Bool.False -> case (x,y,z) of
      (~z,_,_) -> Triangle.Equilateral
      (~y,_,_) -> Triangle.Isosceles
      (_,~z,_) -> Triangle.Isosceles
      _        -> Triangle.Scalene
    ;
  ; where
    (x,y,z) = ...
  ;
;
|] ++ prelude)
        `shouldBe` "(Triangle.Failure,Triangle.Scalene,Triangle.Isosceles,Triangle.Equilateral)"

      it "sort3" $         -- pg 35
        evalString ([r|
main = ( analyse (sort3 (add(twenty,ten),twenty,ten))  ,
         analyse (sort3 (ten,add(twenty,five),twenty)) ,
         analyse (sort3 (ten,twenty,twenty))           ,
         analyse (sort3 (ten,ten,ten))                 )

twenty = mul (two,ten)

--data Triangle
--data Triangle.Failure
--data Triangle.Isosceles
--data Triangle.Equilateral
--data Triangle.Scalene

sort3 = func :: ((Nat,Nat,Nat) -> (Nat,Nat,Nat)) ->
  case ( lt(x,y) , lt(x,z) , lt(y,x) , lt(y,z) ) of
    (Bool.True, Bool.True, _, Bool.True)  -> (x,y,z)
    (Bool.True, Bool.True, _, Bool.False) -> (x,z,y)
    (_, Bool.True,  Bool.True, Bool.True) -> (y,x,z)
    (_, Bool.False, Bool.True, Bool.True) -> (y,z,x)
    (Bool.True,  _, _, _)                 -> (z,x,y)
    (Bool.False, _, _, _)                 -> (z,y,x)
  ; where
    (x,y,z) = ...
  ;
;

analyse = func :: ((Nat,Nat,Nat) -> Triangle) ->
  case lte (add (x,y), z) of
    Bool.True  -> Triangle.Failure
    Bool.False -> case (x,y,z) of
      (~z,_,_) -> Triangle.Equilateral
      (~y,_,_) -> Triangle.Isosceles
      (_,~z,_) -> Triangle.Isosceles
      _        -> Triangle.Scalene
    ;
  ; where
    (x,y,z) = ...
  ;
;
|] ++ prelude)
        `shouldBe` "(Triangle.Failure,Triangle.Scalene,Triangle.Isosceles,Triangle.Equilateral)"

-------------------------------------------------------------------------------

    describe "Chapter 2.2 - Characters:" $ do                 -- pg 35

      it "char" $         -- pg 36
        evalString ([r|
main = (xeq,xgt,cs,low,cp1,cp2,nx,ok)

xeq = not (matches (c1,c2))
xgt = lt (c1,c2)
cs  = lt (Char.AA, Char.Aa)
low = and (not (isLower Char.AA), isLower Char.Dd)
cp1 = matches (capitalize Char.BB, Char.BB)
cp2 = matches (capitalize Char.Cc, Char.CC)
nx  = and (and (nx1,nx2) , and (nx3,nx4))
nx1 = matches (nextlet Char.Cc, Char.Dd)
nx2 = matches (nextlet Char.AA, Char.BB)
nx3 = matches (nextlet Char.DD, Char.AA)
nx4 = matches (nextlet Char.Dd, Char.Aa)
ok  = matches (sum,ten)
sum = add (add (add (ord c1,ord c2), ord Char.CC), ord Char.DD)
c1  = Char.AA
c2  = Char.BB
|] ++ prelude)
        `shouldBe` "(Bool.True,Bool.True,Bool.True,Bool.True,Bool.True,Bool.True,Bool.True,Bool.True)"

-------------------------------------------------------------------------------

    describe "Chapter 2.3 - Enumerations:" $ do                 -- pg 38

      it "enum" $         -- pg 38
        evalString ([r|
main = (xeq, xgt, matches (sum,ten), matches (dsat,Day.Sat), not wd1, wd2, aft)

dsat :: Day = fromEnum (toEnum Day.Sat)

--data Day
--data Day.Sun
--data Day.Mon
--data Day.Tue
--data Day.Wed
--data Day.Thu
--data Day.Fri
--data Day.Sat

xeq = not (matches (d1,d2))
xgt = lt  (d1, d2)
wd1 = workday Day.Sun
wd2 = workday Day.Fri
aft = and (matches (dayAfter Day.Sun,Day.Mon), matches (dayAfter Day.Sat, Day.Sun))
sum = add (add (add (toEnum d1,toEnum d2),toEnum Day.Sat), toEnum Day.Wed)

d1 = Day.Sun
d2 = Day.Mon

workday = func ->
  let day = ... in
    and (gte (day,Day.Mon), lte(day,Day.Fri))
  ;
;

dayAfter = func :: (Day -> Day) ->
  let day = ... in
    fromEnum (rem (add (toEnum day,one), seven))
  ;
;

implementation of IEnum for Day with
  toEnum = func ->
    case ... of
      Day.Sun -> zero
      Day.Mon -> one
      Day.Tue -> two
      Day.Wed -> three
      Day.Thu -> four
      Day.Fri -> five
      Day.Sat -> six
    ;
  ;

  fromEnum = func ->
    case ... of
      ~zero  -> Day.Sun
      ~one   -> Day.Mon
      ~two   -> Day.Tue
      ~three -> Day.Wed
      ~four  -> Day.Thu
      ~five  -> Day.Fri
      ~six   -> Day.Sat
    ;
  ;
;

implementation of IOrd for Day with
  lt = func :: ((Day,Day) -> Bool) ->
    let
      (x,y) = ...
    in
      lt (x', y') where
        x' = toEnum x
        y' = toEnum y
      ;
    ;
  ;
;

implementation of IEq for Day with
  eq = func ->
    matches ...
  ;
;
|] ++ prelude)
        `shouldBe` "(Bool.True,Bool.True,Bool.True,Bool.True,Bool.True,Bool.True,Bool.True)"

      it "direction" $         -- pg 41
        evalString ([r|
main = (toEnum Dir.N, ses, matches (lel, Dir.L), matches (reverse Dir.O, Dir.L))

ses :: Dir = fromEnum one

lel :: Dir = fromEnum (toEnum l)
l   = Dir.L

--data Dir
--data Dir.N
--data Dir.S
--data Dir.L
--data Dir.O

implementation of IEnum for Dir with
  toEnum = func ->
    case ... of
      Dir.N -> zero
      Dir.S -> one
      Dir.L -> two
      Dir.O -> three
    ;
  ;

  fromEnum = func ->
    case ... of
      ~zero  -> Dir.N
      ~one   -> Dir.S
      ~two   -> Dir.L
      ~three -> Dir.O
    ;
  ;
;

reverse = func ->
   case ... of
    Dir.N -> Dir.S
    Dir.S -> Dir.N
    Dir.L -> Dir.O
    Dir.O -> Dir.L
  ;
;
|] ++ prelude)
        `shouldBe` "(Nat.Zero,Dir.S,Bool.True,Bool.True)"

      it "bool enum" $         -- pg 41
        evalString ([r|
main :: Bool = fromEnum (add (toEnum Bool.False, one))
|] ++ prelude)
        `shouldBe` "Bool.True"

-------------------------------------------------------------------------------

    describe "Chapter 2.4 - Tuples:" $ do                     -- pg 41

      it "mkpair" $         -- pg 41
        evalString ("main = Pair (one,two)" ++ nat)
        `shouldBe` "(Pair ((Nat.Succ Nat.Zero),(Nat.Succ (Nat.Succ Nat.Zero))))"

{-
      -- TODO: type compatibility ECons/EFunc
      it "mkpair" $         -- pg 41
        (run True $
          unlines [
            "data Pair for (a,b) with (a,b)",
            "var mkPair : ((a,b) -> Pair of (a,b)) = Pair",
            "var p1 : Pair of (Nat,Nat) = mkPair (1,2)",
            "return p1"
           ])
        `shouldBe` Right (EData ["Pair"] (ETuple [EData ["Nat","1"] EUnit,EData ["Nat","2"] EUnit]))
-}

      it "fst/snd" $         -- pg 41
        evalString ([r|
main = and (matches (add (fst(one,zero),snd(zero,two)), three), snd (Bool.False,Bool.True))
fst = func -> x where (x,_)=... ;;
snd = func -> y where (_,y)=... ;;
|] ++ prelude)
        `shouldBe` "Bool.True"

      it "pair" $         -- pg 42
        evalString ([r|
main = pair ((f,g), one)
f = func -> matches (add (zero,...), ...) ;
g = func -> mul (two, ...) ;
pair = func -> (f x, g x) where
  ((f,g),x) = ...
;;
|] ++ prelude)
        `shouldBe` "(Bool.True,(Nat.Succ (Nat.Succ Nat.Zero)))"

      it "compose" $         -- pg 15
        evalString ([r|
main    = (compose (dec, compose (dup,Nat.Succ))) one
dup     = func -> mul (two,...) ;
compose = func ->
  func {f,g} ->
    f (g ...)
  ; where
    (f,g) = ...
  ;
;
|] ++ prelude)
          `shouldBe` "(Nat.Succ (Nat.Succ (Nat.Succ Nat.Zero)))"

{-
      it "cross" $         -- pg 42
        (run True $
          pre ++ unlines [
            "func fst (x,y) : ((a,b) -> a) do",
            "   return x",
            "end",
            "func snd (_,y) : ((a,b) -> a) do",
            "   return y",
            "end",
            "func cross ((f,g), p) : ((((a->b),(c->d)),(a,c)) -> (b,d)) do",
            "   return (f (fst p), g (snd p))",
            "end",
            "func f x : (Nat -> Bool) do",
            "   return (x rem 2) === 1",
            "end",
            "func g x : (Nat -> Nat) do",
            "   return x * 2",
            "end",
            "return cross ((f,g), (3,4))"
           ])
        `shouldBe` Right (ETuple [EData ["Bool","True"] EUnit,EData ["Nat","8"] EUnit])

      it "pi" $         -- pg 44
        (run True $
          pre ++ unlines [
            "func pifun : (() -> Nat) do",
            "   return 314",
            "end",
            "return pifun ()"
           ])
        `shouldBe` Right (EData ["Nat","314"] EUnit)

      it "roots" $         -- pg 44
        (run True $
          pre ++ unlines [
            "implementation of IEqualable for (Nat,Nat) with end",
            "func sqrt x : (Nat -> Nat) do",
            "  if (x === 0) or (x === 1) then",
            "    return x; ",
            "  end",
            "",
            "  var i : Nat = 1",
            "  var r : Nat = 1",
            "  loop do",
            "    if r @> x then",
            "      return i - 1",
            "    else",
            "      set i = i + 1",
            "      set r = i * i",
            "    end",
            "  end",
            "end",
            "func roots (a,b,c) : ((Nat,Nat,Nat) -> (Nat,Nat)) do",
            "   var e : Nat = (b*b) - (4 * (a*c))",
            "   if a === 0 then",
            "     return (0,0)",
            "   else/if e @< 0 then",
            "     return (-1,-1)",
            "   else",
            "     var d : Nat = 2 * a",
            "     var r : Nat = sqrt e",
            "     return (((-b)-r)/d, ((-b)+r)/d)",
            "   end",
            "end",
            "return (((roots (3,4,5)) === (-1,-1)) and ((roots (2,-16,-18)) === (-1,9))) and ((roots (0,1,1)) === (0,0))",
            "//return (roots (2,-16,-18))",
            ""
           ])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

      it "tuples / eq" $         -- pg 45
        (run True $
          pre ++ unlines [
            "implementation of IEqualable for (a,b) with end",
            "return (((1,1)===(1,1)) and ((1,2)=/=(1,1)))"
           ])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

      it "tuples / ord" $         -- pg 45
        (run True $
          pre ++ unlines [
            "implementation of IEqualable for (a,b) with end",
            "implementation of IOrderable for (a,b) where (a is IOrderable,b is IOrderable) with",
            "   func @< ((i,j),(x,y)) : (((a,b),(a,b)) -> Bool) do",
            "     return (i @< x) or ((i === x) and (j @< y))",
            "   end",
            "end",
            "return ((1,1) === (1,1)) and (((1,1) @< (1,2)) and ((1,2) @< (2,1)))"
           ])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

      it "Triple" $         -- pg 45
        (run True $
          pre ++ unlines [
            "data Triple for (a,b,c) with (a,b,c)",
            "implementation of IEqualable for Triple of (a,b,c) with end",
            "var t0 : Triple of (Nat,Nat,Nat) = Triple (1,0,3)",
            "var t1 : Triple of (Nat,Nat,Nat) = Triple (1,2,3)",
            "var t2 : Triple of (Nat,Nat,Nat) = Triple (1,2,3)",
            "return (t1 === t2) and (t0 =/= t1)"
           ])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

      it "Triple" $         -- pg 45
        (run True $
          pre ++ unlines [
            "data Triple for (a,b,c) with (a,b,c)",
            "implementation of IEqualable for Triple with end",
            "var t1 : Triple of (Nat,Nat,Nat)    = Triple (1,2,3)",
            "var t2 : Triple of (Bool,Bool,Bool) = Triple (Bool.True,Bool.False,Bool.True)",
            "return t1 =/= t2"
           ])
        `shouldBe` Left "(line 79, column 11):\ntypes do not match : expected '(((Triple of (Nat,Nat,Nat)),(Triple of (Bool,Bool,Bool))) -> ?)' : found '((a,a) -> Bool)'\n(line 79, column 11):\nambiguous implementations for 'a' : '(Triple of (Nat,Nat,Nat))', '(Triple of (Bool,Bool,Bool))'\n"
        --`shouldBe` Left "(line 79, column 11):\nvariable '=/=' has no associated implementation for '(((Triple of (Nat,Nat,Nat)),(Triple of (Bool,Bool,Bool))) -> ?)'\n"

      it "Date - age" $         -- pg 45
        (run True $
          pre ++ unlines [
            "implementation of IEqualable for (a,b) with end",
            "implementation of IOrderable for (a,b) where (a is IOrderable,b is IOrderable) with",
            "   func @< ((i,j),(x,y)) : (((a,b),(a,b)) -> Bool) do",
            "     return (i @< x) or ((i === x) and (j @< y))",
            "   end",
            "end",
            "data Date with (Nat,Nat,Nat)",
            "func age (now,person) : ((Date,Date) -> Nat) do",
            "   var (d1,m1,y1) : (Nat,Nat,Nat)",
            "   var (d2,m2,y2) : (Nat,Nat,Nat)",
            "   set Date (d2,m2,y2) = now",
            "   set Date (d1,m1,y1) = person",
            "   if (d1,m1) @< (d2,m2) then",
            "     return (y2-y1)-1",
            "   else",
            "     return y2-y1",
            "   end",
            "end",
            "return ( (age(Date(4,6,2019), Date(3,6,1979))) +",
            "         (age(Date(3,6,2019), Date(3,6,1979))) ) +",
            "         (age(Date(2,6,2019), Date(3,6,1979)))"
           ])
        `shouldBe` Right (EData ["Nat","119"] EUnit)

-------------------------------------------------------------------------------

    describe "Chapter 2.5 - Other Types:" $ do                  -- pg 46

      it "Either" $         -- pg 46
        (run True $
          pre ++ [r|
data Either
data Either.Left  with Bool
data Either.Right with Nat
var l : Either.Left  = Either.Left  Bool.True
var r : Either.Right = Either.Right 10
return (l,r)
|])
        `shouldBe` Right (ETuple [EData ["Either","Left"] (EData ["Bool","True"] EUnit),EData ["Either","Right"] (EData ["Nat","10"] EUnit)])

      it "Either a b" $         -- pg 46
        (run True $
          pre ++ [r|
data Either for (a,b)
data Either.Left  with a
data Either.Right with b
var l : Either.Left  of (Bool,Nat) = Either.Left  Bool.True
var r : Either.Right of (Bool,Nat) = Either.Right 10
return (l,r)
|])
        `shouldBe` Right (ETuple [EData ["Either","Left"] (EData ["Bool","True"] EUnit),EData ["Either","Right"] (EData ["Nat","10"] EUnit)])

      it "case" $         -- pg 46
        (run True $
          pre ++ [r|
data Either for (a,b) is abstract
data Either.Left  with a
data Either.Right with b

func case_ ((f,g),v) : ((((a->r),(b->r)), Either of (a,b)) -> r) do
  match v with
    case (Either.Left  =x) : Nat then
      return f x
    case (Either.Right =x) : Nat then
      return g x
  end
end

func f (v) : (Bool -> Nat) do
  return 1
end

func g (v) : (Nat -> Nat) do
  return 10
end

var l : Either.Left  of (Bool,Nat) = Either.Left  Bool.True
var r : Either.Right of (Bool,Nat) = Either.Right 10

return (case_ ((f,g),l)) + (case_ ((f,g),r))
|])
        `shouldBe` Right (EData ["Nat","11"] EUnit)

      it "Either / IEq" $         -- pg 47
        (run True $
          pre ++ [r|
data Either for (a,b)
data Either.Left  with a
data Either.Right with b

implementation of IEqualable for Either of (a,b) with end

var l : Either.Left  of (Bool,Nat) = Either.Left  Bool.True
var r : Either.Right of (Bool,Nat) = Either.Right 10

var l_ : Either of (Bool,Nat) = l

return (l_ === l) and (l_ =/= r)
|])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

      it "Either / IOrd" $         -- pg 47
        (run True $
          pre ++ [r|
data Either for (a,b) is abstract
data Either.Left  with a
data Either.Right with b

implementation of IEqualable for Either of (a,b) with end
implementation of IOrderable for Either of (a,b) where (a is IOrderable, b is IOrderable) with
  func @< (x,y) : ((Either of (a,b), Either of (a,b)) -> Bool) do
    match (x,y) with
      case (Either.Left =xl,  Either.Left =yl)  : (a,a) then
        return xl @< yl
      case (Either.Left _,    Either.Right _)           then
        return Bool.True
      case (Either.Right =xr, Either.Right =yr) : (b,b) then
        return xr @< yr
      case (Either.Right _,   Either.Left  _)           then
        return Bool.False
    end
  end
end

var f : Either.Left  of (Bool,Nat) = Either.Left  Bool.False
var l : Either.Left  of (Bool,Nat) = Either.Left  Bool.True
var r : Either.Right of (Bool,Nat) = Either.Right 10

var f_ : Either of (Bool,Nat) = f
var l_ : Either of (Bool,Nat) = l
var r_ : Either of (Bool,Nat) = r

return (f_ @<= f) and (((f @< l_) and (l @< r_)) and (r_ @>= r))
|])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

-------------------------------------------------------------------------------

    describe "Chapter 2.6 - Type Synonyms:" $ do                -- pg 48

      it "Roots/Coefs" $         -- pg 48
        (run True $
          pre ++ [r|
data Coefs with (Nat,Nat,Nat)
data Roots with (Nat,Nat)

implementation of IEqualable for Roots with end

func sqrt x : (Nat -> Nat) do
  if (x === 0) or (x === 1) then
    return x;
  end

  var i : Nat = 1
  var r : Nat = 1
  loop do
    if r @> x then
      return i - 1
    else
      set i = i + 1
      set r = i * i
    end
  end
end

func roots (cs) : (Coefs -> Roots) do
  var (a,b,c) : (Nat,Nat,Nat)
  set Coefs (a,b,c) = cs
  var e : Nat = (b*b) - (4 * (a*c))
  if a === 0 then
    return Roots (0,0)
  else/if e @< 0 then
    return Roots (-1,-1)
  else
    var d : Nat = 2 * a
    var r : Nat = sqrt e
    return Roots (((-b)-r)/d, ((-b)+r)/d)
  end
end

return (((roots(Coefs (3,4,5))) === (Roots (-1,-1))) and ((roots(Coefs (2,-16,-18))) === (Roots (-1,9)))) and ((roots(Coefs (0,1,1))) === (Roots (0,0)))
|])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

      it "move" $         -- pg 48
        (run True $
          pre ++ [r|
data Position with (Nat,Nat)
data Angle    with Nat
data Distance with Nat

func move (d_,a_,p_) : ((Distance,Angle,Position) -> Position) do
  var (d,a,x,y) : (Nat,Nat,Nat,Nat)
  set Distance d     = d_
  set Angle a        = a_
  set Position (x,y) = p_
  return Position (x+(d*a), y+(d*(-a)))  // TODO: sin/cos
end

return move(Distance 10,Angle 2,Position(0,0))
|])
        `shouldBe` Right (EData ["Position"] (ETuple [EData ["Nat","20"] EUnit,EData ["Nat","-20"] EUnit]))

      it "OneTwo" $         -- pg 49
        (run True $
          [r|
data Pair   for a with (a,a)
data OneTwo for a
data OneTwo.One with a
data OneTwo.Two with Pair of a
return (OneTwo.One 10, OneTwo.Two (Pair (10,20)))
|])
        `shouldBe` Right (ETuple [EData ["OneTwo","One"] (EData ["Nat","10"] EUnit),EData ["OneTwo","Two"] (EData ["Pair"] (ETuple [EData ["Nat","10"] EUnit,EData ["Nat","20"] EUnit]))])

      it "OneTwo" $         -- pg 49
        (run True $
          [r|
data Pair   for a with (a,a)
data OneTwo for a
data OneTwo.One with a
data OneTwo.Two with Pair of a
return (OneTwo.One 10, OneTwo.Two (Pair (10,())))
|])
        `shouldBe` Left "(line 6, column 36):\ntypes do not match : expected '((Nat,()) -> ?)' : found '((a,a) -> (Pair of a))'\n(line 6, column 36):\nambiguous instances for 'a' : 'Nat', '()'\n"

      it "Angle" $         -- pg 49
        (run True $
          pre ++ [r|
data Angle with Nat

implementation of IEqualable for Angle with
  func === (a1,a2) : ((Angle,Angle) -> Bool) do
    var (a1_,a2_) : (Nat,Nat)
    set (Angle a1_, Angle a2_) = (a1,a2)
    return (a1_ rem 360) == (a2_ rem 360)
  end
end

var a1 : Angle = Angle 370
var a2 : Angle = Angle  10
//return a1 === a2
return ((Angle 370) === (Angle 10)) and (a1 === a2)
|])
        `shouldBe` Right (EData ["Bool","True"] EUnit)

      it "Distance" $         -- pg 50
        (run True $
          pre ++ [r|
data Distance with Nat

implementation of IEqualable for Distance with
  func === (d1,d2) : ((Distance,Distance) -> Bool) do
    var (d1_,d2_) : (Nat,Nat)
    set (Distance d1_, Distance d2_) = (d1,d2)
    if d2_ < d1_ then
      return (d1_-d2_) < 10
    else
      return (d2_-d1_) < 10
    end
  end
end

return ((Distance 10) === (Distance 11), (Distance 11) === (Distance 10),
        (Distance 20) =/= (Distance 10), (Distance 10) =/= (Distance 20))
|])
        `shouldBe` Right (ETuple [EData ["Bool","True"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","True"] EUnit])

-------------------------------------------------------------------------------

    describe "Chapter 2.7 - Strings:" $ do                      -- pg 50

      it "TODO: STRINGS" $    -- pg 50
        (1 `shouldBe` 2)

-------------------------------------------------------------------------------

  describe "Chapter 3 - Numbers:" $ do                          -- pg 50

-------------------------------------------------------------------------------

    describe "Chapter 3.1 - Natural Numbers:" $ do              -- pg 50

      it "Nat" $              -- pg 57
        (run True $
          [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat
return Nat.Succ (Nat.Zero)
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit))

      it "Nat +" $            -- pg 58
        (run True $
          [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

func ++ (x,y) : ((Nat,Nat) -> Nat) do
  if y matches Nat.Zero then
    return x
  else
    var z : Nat
    set! Nat.Succ z = y
    return Nat.Succ (x ++ z)
  end
end

return (Nat.Zero) ++ (Nat.Succ (Nat.Zero))
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit))

      it "Nat *" $            -- pg 59
        (run True $
          nat ++ [r|
return ((zero ** one) ++ (one ** one)) ++ ((one++one) ** (one++one))
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit))))))

      it "Nat ^" $            -- pg 59
        (run True $
          nat ++ [r|
func ^^ (x,y) : ((Nat,Nat) -> Nat) do
  if y matches Nat.Zero then
    return Nat.Succ (Nat.Zero)
  else
    var z : Nat
    set! Nat.Succ z = y
    return (x ^^ z) ** x
  end
end

return ((one++one) ^^ zero) ++ ((one++one) ^^ ((one++one)++one))
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit))))))))))

      it "Nat : Eq" $            -- pg 59
        (run True $
          pre ++ [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

implementation of IEqualable for Nat with
  func === (x,y) : ((Nat,Nat) -> Bool) do
    if x matches Nat.Zero then
      if y matches Nat.Zero then
        return Bool.True
      else
        return Bool.False
      end
    else/if y matches Nat.Zero then
        return Bool.False
    else
      var (x_,y_) : (Nat,Nat)
      set! Nat.Succ x_ = x
      set! Nat.Succ y_ = y
      return x_ === y_
    end
  end
end

var zero : Nat = Nat.Zero
var one : Nat = Nat.Succ (Nat.Zero)

return (zero === zero, zero =/= one, one =/= zero, one === one, one =/= one)
|])
        `shouldBe` Right (ETuple [EData ["Bool","True"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","False"] EUnit])

      it "Nat : Ord" $            -- pg 59
        (run True $
          pre ++ [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

implementation of IEqualable for Nat with
  func === (x,y) : ((Nat,Nat) -> Bool) do
    if x matches Nat.Zero then
      if y matches Nat.Zero then
        return Bool.True
      else
        return Bool.False
      end
    else/if y matches Nat.Zero then
        return Bool.False
    else
      var (x_,y_) : (Nat,Nat)
      set! Nat.Succ x_ = x
      set! Nat.Succ y_ = y
      return x_ === y_
    end
  end
end

implementation of IOrderable for Nat with
  func @< (x,y) : ((Nat,Nat) -> Bool) do
    if x matches Nat.Zero then
      if y matches Nat.Zero then
        return Bool.False
      else
        return Bool.True
      end
    else/if y matches Nat.Zero then
        return Bool.False
    else
      var (x_,y_) : (Nat,Nat)
      set! Nat.Succ x_ = x
      set! Nat.Succ y_ = y
      return x_ @< y_
    end
  end
end

var zero : Nat = Nat.Zero
var one : Nat = Nat.Succ (Nat.Zero)

return (zero @< zero, zero @<= zero, zero @< one, one @< zero, one @> one, one @>= one)
|])
        `shouldBe` Right (ETuple [EData ["Bool","False"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","False"] EUnit,EData ["Bool","False"] EUnit,EData ["Bool","True"] EUnit])

      it "Nat -" $            -- pg 60
        (run True $
          [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

func -- (x,y) : ((Nat,Nat) -> Nat) do
  if y matches Nat.Zero then
    return x
  else
    var (x_,y_) : (Nat,Nat)
    set! Nat.Succ x_ = x
    set! Nat.Succ y_ = y
    return Nat.Succ (x_ -- y_)
  end
end

var zero : Nat = Nat.Zero
var one  : Nat = Nat.Succ (Nat.Zero)

return one--zero
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit))

      it "Nat -" $            -- pg 60
        (run True $
          [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

func -- (x,y) : ((Nat,Nat) -> Nat) do
  if y matches Nat.Zero then
    return x
  else
    var (x_,y_) : (Nat,Nat)
    set! Nat.Succ x_ = x
    set! Nat.Succ y_ = y
    return Nat.Succ (x_ -- y_)
  end
end

var zero : Nat = Nat.Zero
var one : Nat = Nat.Succ (Nat.Zero)

return zero--one
|])
        `shouldBe` Right (EError (-2))

      it "fact" $            -- pg 60
        (run True $
          nat ++ [r|
func fact (x) : (Nat -> Nat) do
  if x matches zero then
    return one
  else
    var z : Nat
    set! Nat.Succ z = x
    return x ** (fact z)
  end
end

return fact (one ++ (one ++ one))
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit)))))))

      it "fib" $            -- pg 61
        (run True $
          nat ++ [r|
func fib (x) : (Nat -> Nat) do
  if x matches zero then
    return zero
  else/if x matches one then
    return one
  else
    var z : Nat
    set! Nat.Succ (Nat.Succ z) = x
    return (fib (Nat.Succ z)) ++ (fib z)
  end
end

return fib (one ++ (one ++ (one ++ (one ++ (one ++ one)))))
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit)))))))))

      it "convert" $            -- pg 62
        (run True $
          nat ++ [r|
func convert (x) : (Nat -> Nat) do
  if x matches Nat.Zero then
    return 0
  else
    var z : Nat
    set! Nat.Succ z = x
    return 1 + (convert z)
  end
end

return convert (one ++ (one ++ (one ++ (one ++ (one ++ one)))))
|])
        `shouldBe` Right (EData ["Nat","6"] EUnit)

      it "Nat -" $            -- pg 60
        (run True $
          [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

func -- (x,y) : ((Nat,Nat) -> Nat) do
  if y matches Nat.Zero then
    return x
  else/if x matches Nat.Zero then
    return x
  else
    var (x_,y_) : (Nat,Nat)
    set! Nat.Succ x_ = x
    set! Nat.Succ y_ = y
    return Nat.Succ (x_ -- y_)
  end
end

var zero : Nat = Nat.Zero
var one : Nat = Nat.Succ (Nat.Zero)

return zero--one
|])
        `shouldBe` Right (EData ["Nat","Zero"] EUnit)

      it "Nat +/*" $            -- pg 58
        (run True $
          [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

func ++ (x,y) : ((Nat,Nat) -> Nat) do
  if x matches Nat.Zero then
    return y
  else
    var z : Nat
    set! Nat.Succ z = x
    return Nat.Succ (y ++ z)
  end
end

func ** (x,y) : ((Nat,Nat) -> Nat) do
  if x matches Nat.Zero then
    return Nat.Zero
  else
    var z : Nat
    set! Nat.Succ z = x
    return (y ** z) ++ y
  end
end

var zero : Nat = Nat.Zero
var one : Nat = Nat.Succ (Nat.Zero)

return ((Nat.Zero) ++ (Nat.Succ (Nat.Zero)),
        ((zero ** one) ++ (one ** one)) ++ ((one++one) ** (one++one)))
|])
        `shouldBe` Right (ETuple [EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit),EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit)))))])

-------------------------------------------------------------------------------

  --describe "Chapter 3.2 - Induction:" $ do                  -- pg 63

-------------------------------------------------------------------------------

    describe "Chapter 3.3 - The fold Function:" $ do            -- pg 70

      it "fold : +" $            -- pg 71
        (run True $
          nat ++ [r|
func foldn (h,c,n) : (((a -> a), a, Nat) -> a) do
  if n matches Nat.Zero then
    return c
  else
    var n_ : Nat
    set! Nat.Succ n_ = n
    return h (foldn (h,c,n_))
  end
end

func +++ (x,y) : ((Nat,Nat) -> Nat) do
  return foldn ( (func (n) : (Nat -> Nat) do return Nat.Succ n end), x, y)
end

return one +++ (one +++ one)
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit))))

      it "foldn : +" $            -- pg 71
        (run True $
          nat ++ [r|
func foldn (h,c,n) : (((a -> a), a, Nat) -> a) do
  if n matches Nat.Zero then
    return c
  else
    var n_ : Nat
    set! Nat.Succ n_ = n
    return h (foldn (h,c,n_))
  end
end

func +++ (x,y) : ((Nat,Nat) -> Nat) do
  return foldn (Nat.Succ, x, y)
end

return one +++ (one +++ one)
|])
        `shouldBe` Right (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Succ"] (EData ["Nat","Zero"] EUnit))))

      it "TODO: closures : fold : +/*/^ / more examples" $            -- pg 71
        (run True $
          nat ++ [r|
error 1
|])
        `shouldBe` Right (EData ["Nat","Zero"] EUnit)

-------------------------------------------------------------------------------

    describe "Chapter 3.4 - Haskell Numbers:" $ do            -- pg 75

      it "data Nateger / Positive" $            -- pg 75
        (run True $
          [r|
data Positive
data Positive.One
data Positive.Succ with Positive

data Nateger
data Nateger.Zero
data Nateger.Neg with Positive
data Nateger.Pos with Positive

return (Nateger.Neg (Positive.One), Nateger.Zero, Nateger.Pos (Positive.Succ (Positive.One)))
|])
        `shouldBe` Right (ETuple [EData ["Nateger","Neg"] (EData ["Positive","One"] EUnit),EData ["Nateger","Zero"] EUnit,EData ["Nateger","Pos"] (EData ["Positive","Succ"] (EData ["Positive","One"] EUnit))])

      it "INumeric" $                           -- pg 76
        (run True $
          pre ++ [r|
interface INumeric for a where (a is IEqualable) with
  func ++  : ((a,a) -> a)
  func **  : ((a,a) -> a)
  func neg : (a -> a)

  func -- (x,y) : ((a,a) -> a) do
    return x + (neg y)
  end
end

interface IReal for a where (a is INumeric) with // TODO: , a is IOrderable) with
  //var toRational : (a -> Rational)
end

interface INategral for a where (a is IReal) with // TODO: , a is IEnumerable) with
  func div       : ((a,a) -> a)
  func mod       : ((a,a) -> a)
  //func toNateger : (a -> Nateger)
end

interface IFractional for a where (a is INumeric) with
  func /- : ((a,a) -> a)
  //var fromRational : (a -> Rational)
end

return ()
|])
        `shouldBe` Right EUnit

    describe "Chapter 3.5 - Example: the rationals:" $ do            -- pg 78

      it "Rational" $                           -- pg 78 (TODO: show)
        (run True $
          pre ++ [r|
data Rational with (Nat,Nat)

func signum x : (Nat -> Nat) do
  if x == 0 then
    return 0
  else/if x < 0 then
    return -1
  else
    return 1
  end
end

func abs x : (Nat -> Nat) do
  if x < 0 then
    return -x
  else
    return x
  end
end

func gcd (x,y) : ((Nat,Nat) -> Nat) do
  func gcd_ (a,b) : ((Nat,Nat) -> Nat) do
    if b == 0 then
      return a
    else
      return gcd_ (b, a rem b)
    end
  end
  return gcd_ (abs x, abs y)
end

func mkRat (x,y) : ((Nat,Nat) -> Rational) do
  var u : Nat = (signum y) * x
  var v : Nat = abs y
  var d : Nat = gcd (u,v)
  return Rational (u / d, v / d)
end

return (mkRat (10,2), mkRat (0,1), mkRat (1,-5))
|])
        `shouldBe` Right (ETuple [EData ["Rational"] (ETuple [EData ["Nat","5"] EUnit,EData ["Nat","1"] EUnit]),EData ["Rational"] (ETuple [EData ["Nat","0"] EUnit,EData ["Nat","1"] EUnit]),EData ["Rational"] (ETuple [EData ["Nat","-1"] EUnit,EData ["Nat","5"] EUnit])])

      it "Rational / INumeric / IFractional" $                       -- pg 80
        (run True $
          pre ++ [r|
interface INumeric for a where (a is IEqualable) with
  func ++  : ((a,a) -> a)
  func **  : ((a,a) -> a)
  func neg : (a -> a)

  func -- (x,y) : ((a,a) -> a) do
    return x + (neg y)
  end
end

interface IFractional for a where (a is INumeric) with
  func /- : ((a,a) -> a)
  //var fromRational : (a -> Rational)
end

data Rational with (Nat,Nat)

func signum x : (Nat -> Nat) do
  if x == 0 then
    return 0
  else/if x < 0 then
    return -1
  else
    return 1
  end
end

func abs x : (Nat -> Nat) do
  if x < 0 then
    return -x
  else
    return x
  end
end

func gcd (x,y) : ((Nat,Nat) -> Nat) do
  func gcd_ (a,b) : ((Nat,Nat) -> Nat) do
    if b == 0 then
      return a
    else
      return gcd_ (b, a rem b)
    end
  end
  return gcd_ (abs x, abs y)
end

func mkRat (x,y) : ((Nat,Nat) -> Rational) do
  var u : Nat = (signum y) * x
  var v : Nat = abs y
  var d : Nat = gcd (u,v)
  return Rational (u / d, v / d)
end

implementation of IEqualable for Rational with end

implementation of INumeric for Rational with
  func ++ (x,y) : ((Rational,Rational) -> Rational) do
    var (x1,x2) : (Nat,Nat)
    var (y1,y2) : (Nat,Nat)
    set Rational (x1,x2) = x
    set Rational (y1,y2) = y
    return mkRat ((x1*y2) + (y1*x2), x2*y2)
  end
  func -- (x,y) : ((Rational,Rational) -> Rational) do
    var (x1,x2) : (Nat,Nat)
    var (y1,y2) : (Nat,Nat)
    set Rational (x1,x2) = x
    set Rational (y1,y2) = y
    return mkRat ((x1*y2) - (y1*x2), x2*y2)
  end
  func ** (x,y) : ((Rational,Rational) -> Rational) do
    var (x1,x2) : (Nat,Nat)
    var (y1,y2) : (Nat,Nat)
    set Rational (x1,x2) = x
    set Rational (y1,y2) = y
    return mkRat (x1*y1, x2*y2)
  end
  func neg x : (Rational -> Rational) do
    var (x1,x2) : (Nat,Nat)
    set Rational (x1,x2) = x
    return mkRat (-x1, x2)
  end
end

implementation of IFractional for Rational with
  func /- (x,y) : ((Rational,Rational) -> Rational) do
    var Rational (x1,x2) : (Nat,Nat) = x
    var Rational (y1,y2) : (Nat,Nat) = y
    if y1 < 0 then
      return mkRat ((-x1)*y2, (-x2)*y1)
    else/if y1 == 0 then
      error 1
    else
      return mkRat (x1*y2, x2*y1)
    end
  end
end

return (neg ((((mkRat (10,2)) -- (mkRat (0,1))) ++ (mkRat (1,-5))) ** (mkRat (-5,-1)))) /- (mkRat (24,1))
|])
        `shouldBe` Right (EData ["Rational"] (ETuple [EData ["Nat","-1"] EUnit,EData ["Nat","1"] EUnit]))

-------------------------------------------------------------------------------
    --describe "Chapter 3.6 - Example: linear and binary search:" $ do -- pg 81
    --describe "Chapter 3.7 - Church numbers:" $ do                   -- pg 86
-------------------------------------------------------------------------------

  describe "Chapter 4 - Lists:" $ do           -- pg 91

-------------------------------------------------------------------------------

    describe "Chapter 4.1 - List notation:" $ do       -- pg 91

    describe "Chapter 4.1.1 - List as a datatype:" $ do       -- pg 92

      it "List" $                   -- pg 92
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)
return 10 (List.Cons) List.Nil
|])
        `shouldBe` Right (EData ["List","Cons"] (ETuple [EData ["Nat","10"] EUnit,EData ["List","Nil"] EUnit]))

      it "List" $                   -- pg 92
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)
return List.Cons (10, List.Nil)
|])
        `shouldBe` Right (EData ["List","Cons"] (ETuple [EData ["Nat","10"] EUnit,EData ["List","Nil"] EUnit]))

      it "TODO: List `:´" $                   -- pg 92
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)
func :: : (a -> List of a)
set :: = List.Cons
return 10 :: (List.Nil)
|])
        `shouldBe` Right (EData ["List","Cons"] (ETuple [EData ["Nat","10"] EUnit,EData ["List","Nil"] EUnit]))

      it "List: ==" $                   -- pg 93
        (run True $
          pre ++ [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

implementation of IEqualable for List of a where (a is IEqualable) with end

func eq (l1,l2) : ((List of a, List of a) -> Bool) do
  if l1 matches List.Nil then
    if l2 matches List.Nil then
      return Bool.True
    else
      return Bool.False
    end
  else/if l2 matches List.Nil then
      return Bool.False
  else
      var (v1 ,v2 ) : (a,         a)
      var (l1_,l2_) : (List of a, List of a)
      set! List.Cons (v1,l1_) = l1
      set! List.Cons (v2,l2_) = l2
      return (v1 === v2) and (l1_ === l2_)
  end
end

return ((10 (List.Cons) List.Nil) =/= (List.Cons (10, List.Nil)),
        (10 (List.Cons) List.Nil) eq  (List.Cons (10, List.Nil)),
        (10 (List.Cons) List.Nil) eq  (List.Cons (11, List.Nil)))
|])
        `shouldBe` Right (ETuple [EData ["Bool","False"] EUnit,EData ["Bool","True"] EUnit,EData ["Bool","False"] EUnit])

      it "List: null" $                   -- pg 93
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

func null l : (List of a -> Bool) do
  if l matches List.Nil then
    return Bool.True
  else
    return Bool.False
  end
end

return (null (List.Cons (10, List.Nil)), null (List.Nil))
|])
        `shouldBe` Right (ETuple [EData ["Bool","False"] EUnit,EData ["Bool","True"] EUnit])

      it "List: IOrderable" $                   -- pg 94
        (run True $
          pre ++ [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

implementation of IEqualable for List of a where (a is IEqualable) with end

implementation of IOrderable for (List of a) with
  func @< (xs, ys) : ((List of a, List of a) -> Bool) do
    if xs matches List.Nil then
      return Bool.False
    else/if ys matches List.Nil then
      return Bool.True
    else
      var! List.Cons (x,xs_) : (a, List of a) = xs
      var! List.Cons (y,ys_) : (a, List of a) = ys
      return (x @< y) or ((x === y) and (xs_ @< ys_))
    end
  end
end

func null l : (List of a -> Bool) do
  if l matches List.Nil then
    return Bool.True
  else
    return Bool.False
  end
end

return (null (List.Cons (10, List.Nil)), null (List.Nil))
|])
        `shouldBe` Right (ETuple [EData ["Bool","False"] EUnit,EData ["Bool","True"] EUnit])

      it "List: last1/last2" $                   -- pg 94
        (run True $
          pre ++ [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

implementation of IEqualable for List of a where (a is IEqualable) with end

func null l : (List of a -> Bool) do
  if l matches List.Nil then
    return Bool.True
  else
    return Bool.False
  end
end

func last1 (xs) : (List of a -> a) do
  var x   : a
  var xs_ : List of a
  set! List.Cons (x,xs_) = xs
  if xs_ matches List.Nil then
    return x
  else
    return last1 xs_
  end
end

func last2 (xs) : (List of a -> a) do
  var! List.Cons (x,xs_) : (a, List of a) = xs
  if List.Nil === xs_ then
    return x
  else
    return last2 xs_
  end
end

data X

return (last2 (List.Cons (X, List.Nil)), last1 (List.Cons (10, List.Nil)))
|])
        `shouldBe` Right (ETuple [EData ["X"] EUnit,EData ["Nat","10"] EUnit])

      it "List: last2" $                   -- pg 94
        (run True $
          pre ++ [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

func last2 (xs) : (List of a -> a) do
  var x   : a
  var xs_ : List of a
  set! List.Cons (x,xs_) = xs
  if List.Nil === xs_ then
    return x
  else
    return last2 xs_
  end
end

return 1
|])
        `shouldBe` Left "(line 84, column 15):\nvariable '===' has no associated implementation for '(((List.Nil of a),(List of a)) -> Bool)'\n"

      it "List: Snoc" $                   -- pg 94
        (run True $
          pre ++ [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

implementation of IEqualable for List of a where (a is IEqualable) with end

func head xs : (List of a -> a) do
  var! List.Cons (x,_) : a = xs
  return x
end

data Liste for a
data Liste.Nil
data Liste.Snoc with (Liste of a, a)

func heade xs : (Liste of a -> a) do
  var xs_ : Liste of a
  var x   : a
  set! Liste.Snoc (xs_,x) = xs
  if xs_ matches Liste.Nil then
    return x
  else
    return heade xs_
  end
end

func convert (xs,acc) : ((Liste of a, List of a) -> List of a) do
  if xs matches Liste.Nil then
    return acc
  else
    var! Liste.Snoc (xs_,x) : (Liste of a, a) = xs
    return convert (xs_, List.Cons (x, acc))
  end
end

var l  : List  of Nat = List.Cons  (10, List.Cons  (20, List.Nil ))
var le : Liste of Nat = Liste.Snoc (Liste.Snoc (Liste.Nil, 10), 20)

return (head l, heade le, (convert (le, List.Nil)) === l)
|])
        `shouldBe` Right (ETuple [EData ["Nat","10"] EUnit,EData ["Nat","10"] EUnit,EData ["Bool","True"] EUnit])

-------------------------------------------------------------------------------

    describe "Chapter 4.2 - List operations:" $ do       -- pg 95

      it "List: ++" $                   -- pg 95
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

func cat (xs,ys) : ((List of a, List of a) -> List of a) do
  if xs matches List.Nil then
    return ys
  else
    var! List.Cons (x,xs_) : (a,List of a) = xs
    return List.Cons (x, cat (xs_,ys))
  end
end

return cat (List.Cons (10, List.Cons (20, List.Nil)), List.Nil)
|])
        `shouldBe` Right (EData ["List","Cons"] (ETuple [EData ["Nat","10"] EUnit,EData ["List","Cons"] (ETuple [EData ["Nat","20"] EUnit,EData ["List","Nil"] EUnit])]))

      it "List: concat" $                   -- pg 98
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

func cat (xs,ys) : ((List of a, List of a) -> List of a) do
  if xs matches List.Nil then
    return ys
  else
    var x   : a
    var xs_ : List of a
    set! List.Cons (x,xs_) = xs
    return List.Cons (x, cat (xs_,ys))
  end
end

func concat (xss) : (List of (List of a) -> List of a) do
  if xss matches List.Nil then
    return xss
  else
    var! List.Cons (xs,xss_) : (List of a, List of (List of a)) = xss
    return cat (xs, concat (xss_))
  end
end

return concat (List.Cons (List.Cons(10,List.Nil), List.Cons (List.Cons(20,List.Nil), List.Nil)))
|])
        `shouldBe` Right (EData ["List","Cons"] (ETuple [EData ["Nat","10"] EUnit,EData ["List","Cons"] (ETuple [EData ["Nat","20"] EUnit,EData ["List","Nil"] EUnit])]))

      it "List: reverse" $                   -- pg 99
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

func reverse (xs,acc) : ((List of a, List of a) -> List of a) do
  if xs matches List.Nil then
    return acc
  else
    var xs_ : List of a
    var x   : a
    set! List.Cons (x,xs_) = xs
    return reverse (xs_, List.Cons (x, acc))
  end
end

var l : List of Nat = List.Cons (10, List.Cons (20, List.Nil))

return (reverse (l, List.Nil))
|])
        `shouldBe` Right (EData ["List","Cons"] (ETuple [EData ["Nat","20"] EUnit,EData ["List","Cons"] (ETuple [EData ["Nat","10"] EUnit,EData ["List","Nil"] EUnit])]))

      it "List: length" $                   -- pg 102
        (run True $
          [r|
data List for a is abstract
data List.Nil
data List.Cons with (a, List of a)

func length (xs) : (List of a -> Nat) do
  match xs with
    case List.Nil then
      return 0
    case List.Cons (=x,=xs_) : (a,List of a) then
      return 1 + (length xs_)
  end
end

var l : List of Nat = List.Cons (10, List.Cons (20, List.Nil))

return length l
|])
        `shouldBe` Right (EData ["Nat","2"] EUnit)

      it "List: head/tail" $                   -- pg 102
        (run True $
          [r|
data List for a
data List.Nil
data List.Cons with (a, List of a)

func head (xs) : (List of a -> a) do
  var! List.Cons (x,_) : a = xs
  return x
end

func tail (xs) : (List of a -> List of a) do
  var! List.Cons (_,xs_) : a = xs
  return xs_
end

var l : List of Nat = List.Cons (10, List.Cons (20, List.Nil))

return (head l, tail l)
|])
        `shouldBe` Right (ETuple [EData ["Nat","10"] EUnit,EData ["List","Cons"] (ETuple [EData ["Nat","20"] EUnit,EData ["List","Nil"] EUnit])])

-------------------------------------------------------------------------------

    where
      run :: Bool -> String -> Either String Exp
      run withPrelude input =
        let v = parse prog "" input in
          case v of
            (Right p) ->
              case go $ bool Prelude.id (prelude annz) withPrelude $ p of
                (Left errs) -> Left $ concatMap (\s->s++"\n") errs
                (Right exp) -> Right exp
            (Left  v') -> Left (show v')

      nat = [r|
data Nat
data Nat.Zero
data Nat.Succ with Nat

func ++ (x,y) : ((Nat,Nat) -> Nat) do
  if y matches Nat.Zero then
    return x
  else
    var z : Nat
    set! Nat.Succ z = y
    return Nat.Succ (x ++ z)
  end
end

func ** (x,y) : ((Nat,Nat) -> Nat) do
  if y matches Nat.Zero then
    return Nat.Zero
  else
    var z : Nat
    set! Nat.Succ z = y
    return (x ** z) ++ x
  end
end

var zero  : Nat = Nat.Zero
var one   : Nat = Nat.Succ zero
var two   : Nat = Nat.Succ one
var three : Nat = Nat.Succ two
var four  : Nat = Nat.Succ three
var five  : Nat = Nat.Succ four
|]

      pre = [r|
func not (x) : (Bool->Bool) do
  if x matches Bool.True then
    return Bool.False
  else
    return Bool.True
  end
end
func and (x,y) : ((Bool,Bool)->Bool) do
  if x matches Bool.False then
    return Bool.False
  else
    return y
  end
end
func or (x,y) : ((Bool,Bool)->Bool) do
  if x matches Bool.True then
    return Bool.True
  else
    return y
  end
end

interface IEqualable for a with
  func === (x,y) : ((a,a) -> Bool) do
    if y matches x then
      if x matches y then
        return Bool.True
      else
        return Bool.False
      end
    else
      return Bool.False
    end
  end
  func =/= (x,y) : ((a,a) -> Bool) do
    return not (x === y)
  end
end

implementation of IEqualable for Nat with
  func === (x,y) : ((Nat,Nat) -> Bool) do
    return x == y
  end
end

implementation of IEqualable for Bool with
  func === (x,y) : ((Bool,Bool) -> Bool) do
    return (x and y) or ((not x) and (not y))
  end
end

interface IOrderable for a where (a is IEqualable) with
  func @<        : ((a,a) -> Bool)
  func @<= (x,y) : ((a,a) -> Bool) do return (x @< y) or (x === y) end
  func @>  (x,y) : ((a,a) -> Bool) do return not (x @<= y)         end
  func @>= (x,y) : ((a,a) -> Bool) do return (x @> y) or (x === y) end
end

implementation of IOrderable for Nat with
  func @< (x,y) : ((Nat,Nat) -> Bool) do
    return x < y
  end
end

implementation of IOrderable for Bool with
  func @< (x,y) : ((Bool,Bool) -> Bool) do
    if      (x, y) matches (Bool.False, Bool.False) then return Bool.False
    else/if (x, y) matches (Bool.False, Bool.True)  then return Bool.True
    else/if (x, y) matches (Bool.True,  Bool.False) then return Bool.False
    else/if (x, y) matches (Bool.True,  Bool.True)  then return Bool.False
    end
  end
end
|]
-}

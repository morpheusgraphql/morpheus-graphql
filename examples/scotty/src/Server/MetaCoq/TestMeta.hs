{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances #-}

module Server.MetaCoq.TestMeta where
--import Data.Aeson.QQ
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocument)
import Data.Morpheus.Types (RootResolver (..), Undefined, GQLType)
--import Data.Morpheus.Server.Types ( GQLResolver)

--import Data.Generics.Traversable
--import Data.Generics.Traversable.Zipper
--import Data.Generics.Traversable.TH

import Data.Monoid (Monoid, mempty)
--import Data.Generics.Traversable
--import Data.Bitraversable
--import Data.Bifoldable



--import Data.MonoTraversable
import Data.Traversable
--import Data.MonoTraversable (Element, MonoFoldable(..))
--import Data.MonoTraversable (MonoFunctor(..), MonoFoldable(..), MonoTraversable(..))
import qualified GHC.Real (Integral)
import qualified GHC.Base (Monoid,
                           Semigroup
                          )
import qualified Data.Aeson (Value(..))
--import qualified Data.Aeson.Types (Value(String))
import Data.Data
--import Data.Sequences
--import Data.Generics.Uniplate.Data
--import Data.Generics.Uniplate.Operations
import Prelude(Char, Show, Int, Float, (+), show
               --IO, (.), return
               )



--import GHC.Generics (Generic)
import Data.Aeson (toEncoding, ToJSON, genericToEncoding, defaultOptions, object, (.=) )
import GHC.Char ( chr )  

--import  Control.Lens ((&), mapped, (?~), (.~) )
--oimport           Data.Aeson.Encode.Pretty   (encodePretty)
--import qualified Data.ByteString.Lazy.Char8 as BL8
--import           Data.Proxy
--import           Data.Swagger
--import           Data.Text                  (Text)
--import           Data.Time                  (UTCTime (..), fromGregorian)
--import           Data.Typeable              (Typeable)
import           GHC.Generics



toString :: Byte -> Char
toString x = chr (foldNat (toNat (getNpos (toN x))))

--instance (GHC.Real.Integral (Index T_)) where
--  mod a b = a 
  

data T_ =
   Lzero
 | Level MyString
 | Lvar Nat
  deriving (
    Generic
    ,Show
    ,GQLType
    ,Data
    ,Typeable
    )

--your_folding_function_here = 1
--instance MonoFoldable T_ where
--  ofoldMap f (Level st) = ofoldMap f st
--  ofoldMap _ _          = mempty
  
--instance MonoFoldable T_ where
--  ofoldMap f (Level st) t = your_folding_function_here
--  ofoldr f z (T_ t) = your_right_folding_function_here
--  ofoldl' f z (T_ t) = your_left_folding_function_here
--  ofoldr1Ex f (T_ t) = your_right_folding_function_non_empty_here
--  ofoldl1Ex' f (T_ t) = your_left_folding_function_non_empty_here
--  otoList (T_ t) = your_converting_to_list_function_here
--  oall f (T_ t) = your_all_elements_satisfy_predicate_here
--  oany f (T_ t) = your_any_element_satisfies_predicate_here


data Bool =
   True
 | False
  deriving (Generic, Show, GQLType, Data, Typeable)

--type Test1 = Prod (Prod FileObject MyString) Program
type T =MyString 
data MyString =
   EmptyString
-- | MyString2 Byte MyString
 | String Byte MyString
  deriving ( Generic, Show, GQLType, Data, Typeable
           --, MonoFoldable
           )
--instance GTraversable c T_
data Nat =
  O
  | S Nat
  deriving ( Generic, Show, GQLType, Data, Typeable)
--instance GTraversable c Nat
foldNat :: Nat -> Prelude.Int; foldNat = \case { O -> 0; S n -> 1 Prelude.+ foldNat n }
data N =
   N0
  | Npos Positive
  deriving ( Generic, Show, GQLType, Data, Typeable)

data Positive =
  XI Positive
  | XO Positive
  | XH
  deriving ( Generic, Show, GQLType, Data, Typeable)

getNpos :: N -> Positive
getNpos N0 = XH
getNpos (Npos x)  = x


add :: Nat -> Nat -> Nat
add n m =
  case n of {
   O -> m;
   S p -> S (add p m)}

toNat :: Positive -> Nat
toNat x =
  iterOp add x (S O)

iterOp :: (a1 -> a1 -> a1) -> Positive -> a1 -> a1
iterOp op p a =
  case p of {
   XI p0 -> op a (iterOp op p0 (op a a));
   XO p0 -> iterOp op p0 (op a a);
   XH -> a}

fold_right :: (a2 -> a1 -> a1) -> a1 -> (List a2) -> a1
fold_right f a0 l =
  case l of {
   Nil -> a0;
   Cons b t -> f b (fold_right f a0 t)}

--of_list0 :: p -> Program
--of_list0 _ = OfList
type Elt0 = T_

empty0 :: T10
empty0 =
  empty
empty :: Tree
empty =
  Leaf

of_list0 :: (List Elt0) -> T10
of_list0 l =
  fold_right add4 empty0 l

data Comparison =
   Eq
 | Lt
 | Gt
  deriving ( Generic, Show, GQLType, Data, Typeable)

compare10 :: T3 -> T3 -> Comparison
compare10 l1 l2 =
  case l1 of {
   Lzero -> case l2 of {
             Lzero -> Eq;
             _ -> Lt};
   Level s1 ->
    case l2 of {
     Lzero -> Gt;
     Level s2 -> compare5 s1 s2;
     Lvar _ -> Lt};
   Lvar n -> case l2 of {
              Lvar m -> my_compare n m;
              _ -> Gt}}


compare2 :: Z -> Z -> Comparison
compare2 x y =
  case x of {
   Z0 -> case y of {
          Z0 -> Eq;
          Zpos _ -> Lt;
          Zneg _ -> Gt};
   Zpos x' -> case y of {
               Zpos y' -> compare0 x' y';
               _ -> Gt};
   Zneg x' -> case y of {
               Zneg y' -> compOpp (compare0 x' y');
               _ -> Lt}}
compOpp :: Comparison -> Comparison
compOpp r =
  case r of {
   Eq -> Eq;
   Lt -> Gt;
   Gt -> Lt}
this :: T_0 -> T4
this t =
  t

compare0 :: Positive -> Positive -> Comparison
compare0 =
  compare_cont Eq

compare_cont :: Comparison -> Positive -> Positive -> Comparison
compare_cont r x y =
  case x of {
   XI p ->
    case y of {
     XI q -> compare_cont r p q;
     XO q -> compare_cont Gt p q;
     XH -> Gt};
   XO p ->
    case y of {
     XI q -> compare_cont Lt p q;
     XO q -> compare_cont r p q;
     XH -> Gt};
   XH -> case y of {
          XH -> r;
          _ -> Lt}}


compare27 :: T24 -> T24 -> Comparison
compare27 x y =
  case x of {
   Le n -> case y of {
            Le m -> compare2 n m;
            Eq0 -> Lt};
   Eq0 -> case y of {
           Le _ -> Gt;
           Eq0 -> Eq}}

compare30 :: (Prod (Prod T3 T24) T3) -> (Prod (Prod T3 T24) T3) -> Comparison
compare30 pat pat0 =
  case pat of {
   Pair p l2 ->
    case p of {
     Pair l1 t ->
      case pat0 of {
       Pair p0 l2' ->
        case p0 of {
         Pair l1' t' ->
          case compare10 l1 l1' of {
           Eq -> case compare27 t t' of {
                  Eq -> compare10 l2 l2';
                  x -> x};
           x -> x}}}}}

add4 :: Elt0 -> T10 -> T10
add4 x s =
  add3 x (this s)
compare5 :: T -> T -> Comparison
compare5 =
  compare4
compare4 :: T -> T -> Comparison
compare4 xs ys =
  case xs of {
   EmptyString -> case ys of {
                   EmptyString -> Eq;
                   String _ _ -> Lt};
   String x xs0 ->
    case ys of {
     EmptyString -> Gt;
     String y ys0 -> case compare3 x y of {
                      Eq -> compare4 xs0 ys0;
                      x0 -> x0}}}
compare3 :: Byte -> Byte -> Comparison
compare3 x y =
  compare1 (toN x) (toN y)
compare1 :: N -> N -> Comparison
compare1 n m =
  case n of {
   N0 -> case m of {
          N0 -> Eq;
          Npos _ -> Lt};
   Npos n' -> case m of {
               N0 -> Gt;
               Npos m' -> compare0 n' m'}}
add2 :: Z -> Z -> Z
add2 =
  add1
add_carry :: Positive -> Positive -> Positive
add_carry x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XI (add_carry p q);
     XO q -> XO (add_carry p q);
     XH -> XI (my_succ p)};
   XO p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add0 p q);
     XH -> XO (my_succ p)};
   XH -> case y of {
          XI q -> XI (my_succ q);
          XO q -> XO (my_succ q);
          XH -> XI XH}}
my_succ :: Positive -> Positive
my_succ x =
  case x of {
   XI p -> XO (my_succ p);
   XO p -> XI p;
   XH -> XO XH}

add0 :: Positive -> Positive -> Positive
add0 x y =
  case x of {
   XI p ->
    case y of {
     XI q -> XO (add_carry p q);
     XO q -> XI (add0 p q);
     XH -> XO (my_succ p)};
   XO p ->
    case y of {
     XI q -> XI (add0 p q);
     XO q -> XO (add0 p q);
     XH -> XI p};
   XH -> case y of {
          XI q -> XO (my_succ q);
          XO q -> XI q;
          XH -> XO XH}}
double :: Z -> Z
double x =
  case x of {
   Z0 -> Z0;
   Zpos p -> Zpos (XO p);
   Zneg p -> Zneg (XO p)}

succ_double :: Z -> Z
succ_double x =
  case x of {
   Z0 -> Zpos XH;
   Zpos p -> Zpos (XI p);
   Zneg p -> Zneg (pred_double p)}
pred_double :: Positive -> Positive
pred_double x =
  case x of {
   XI p -> XI (XO p);
   XO p -> XI (pred_double p);
   XH -> XH}
pred_double0 :: Z -> Z
pred_double0 x =
  case x of {
   Z0 -> Zneg XH;
   Zpos p -> Zpos (pred_double p);
   Zneg p -> Zneg (XI p)}

pos_sub :: Positive -> Positive -> Z
pos_sub x y =
  case x of {
   XI p ->
    case y of {
     XI q -> double (pos_sub p q);
     XO q -> succ_double (pos_sub p q);
     XH -> Zpos (XO p)};
   XO p ->
    case y of {
     XI q -> pred_double0 (pos_sub p q);
     XO q -> double (pos_sub p q);
     XH -> Zpos (pred_double p)};
   XH ->
    case y of {
     XI q -> Zneg (XO q);
     XO q -> Zneg (pred_double q);
     XH -> Z0}}

add1 :: Z -> Z -> Z
add1 x y =
  case x of {
   Z0 -> y;
   Zpos x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> Zpos (add0 x' y');
     Zneg y' -> pos_sub x' y'};
   Zneg x' ->
    case y of {
     Z0 -> x;
     Zpos y' -> pos_sub y' x';
     Zneg y' -> Zneg (add0 x' y')}}
max1 :: Z -> Z -> Z
max1 =
  max0
max0 :: Z -> Z -> Z
max0 n m =
  case compare2 n m of {
   Lt -> m;
   _ -> n}



create :: T4 -> T_ -> T4 -> Tree

create l x r =
  Node (add2 (max1 (height l) (height r))
        _1       
       ) l x r
assert_false :: T4 -> T_ -> T4 -> Tree
assert_false =
  create
height :: T4 -> T0
height s =
  
  case s of {
   Leaf -> _0;
   Node h _ _ _ -> h}
ltb0 :: Z -> Z -> Bool
ltb0 =
  ltb

ltb :: Z -> Z -> Bool
ltb x y =
  case compare2 x y of {
   Lt -> True;
   _ -> False}
type T0 = Z

_0 :: Z
_0 =
  Z0

_1 :: Z
_1 =
  Zpos XH

_2 :: Z
_2 =
  Zpos (XO XH)
leb1 :: Z -> Z -> Bool
leb1 =
  leb0
leb0 :: Z -> Z -> Bool
leb0 x y =
  case compare2 x y of {
   Gt -> False;
   _ -> True}


bal :: T4 -> T_ -> T4 -> Tree
bal l x r =
  let {hl = height l} in
  let {hr = height r} in
  case ltb0 (add2 hr _2) hl of {
   True ->
    case l of {
     Leaf -> assert_false l x r;
     Node _ ll lx lr ->
      case leb1 (height lr) (height ll) of {
       True -> create ll lx (create lr x r);
       False ->
        case lr of {
         Leaf -> assert_false l x r;
         Node _ lrl lrx lrr -> create (create ll lx lrl) lrx (create lrr x r)}}};
   False ->
    case ltb0 (add2 hl _2) hr of {
     True ->
      case r of {
       Leaf -> assert_false l x r;
       Node _ rl rx rr ->
        case leb1 (height rl) (height rr) of {
         True -> create (create l x rl) rx rr;
         False ->
          case rl of {
           Leaf -> assert_false l x r;
           Node _ rll rlx rlr ->
            create (create l x rll) rlx (create rlr rx rr)}}};
     False -> create l x r}}

-- fixme renamed from compare
my_compare :: Nat -> Nat -> Comparison
my_compare n m =
  case n of {
   O -> case m of {
         O -> Eq;
         S _ -> Lt};
   S n' -> case m of {
            O -> Gt;
            S m' -> my_compare n' m'}}

add3 :: T_ -> Tree -> Tree
add3 x s =
  case s of {
   Leaf -> Node _1 Leaf x Leaf;
   Node h l y r ->
    case case x of {
          Lzero -> case y of {
                    Lzero -> Eq;
                    _ -> Lt};
          Level s1 ->
           case y of {
            Lzero -> Gt;
            Level s2 -> compare5 s1 s2;
            Lvar _ -> Lt};
          Lvar n -> case y of {
                     Lvar m -> my_compare n m;
                     _ -> Gt}} of {
     Eq -> Node h l y r;
     Lt -> bal (add3 x l) y r;
     Gt -> bal l y (add3 x r)}}

--add9 :: Program -> Program -> Program
--add9 a b = Compose2 (Wrap a) (Wrap b) 
type Elt4 = Prod (Prod T3 T24) T3
this1 :: T_4 -> T26
this1 t =
  t
height0 :: T26 -> T0
height0 s =
  case s of {
   Leaf0 -> _0;
   Node0 h _ _ _ -> h}

create0 :: T26 -> (Prod (Prod T3 T24) T3) -> T26 -> Tree0
create0 l x r =
  Node0 (add2 (max1 (height0 l) (height0 r)) _1) l x r

assert_false0 :: T26 -> (Prod (Prod T3 T24) T3) -> T26 -> Tree0
assert_false0 =
  create0

bal0 :: T26 -> (Prod (Prod T3 T24) T3) -> T26 -> Tree0
bal0 l x r =
  let {hl = height0 l} in
  let {hr = height0 r} in
  case ltb0 (add2 hr _2) hl of {
   True ->
    case l of {
     Leaf0 -> assert_false0 l x r;
     Node0 _ ll lx lr ->
      case leb1 (height0 lr) (height0 ll) of {
       True -> create0 ll lx (create0 lr x r);
       False ->
        case lr of {
         Leaf0 -> assert_false0 l x r;
         Node0 _ lrl lrx lrr ->
          create0 (create0 ll lx lrl) lrx (create0 lrr x r)}}};
   False ->
    case ltb0 (add2 hl _2) hr of {
     True ->
      case r of {
       Leaf0 -> assert_false0 l x r;
       Node0 _ rl rx rr ->
        case leb1 (height0 rl) (height0 rr) of {
         True -> create0 (create0 l x rl) rx rr;
         False ->
          case rl of {
           Leaf0 -> assert_false0 l x r;
           Node0 _ rll rlx rlr ->
            create0 (create0 l x rll) rlx (create0 rlr rx rr)}}};
     False -> create0 l x r}}

add9 :: Elt4 -> T32 -> T32
add9 x s =
  add8 x (this1 s)
add8 :: (Prod (Prod T3 T24) T3) -> Tree0 -> Tree0
add8 x s =
  case s of {
   Leaf0 -> Node0 _1 Leaf0 x Leaf0;
   Node0 h l y r ->
    case case x of {
          Pair p l2 ->
           case p of {
            Pair l1 t ->
             case y of {
              Pair p0 l2' ->
               case p0 of {
                Pair l1' t' ->
                 case compare10 l1 l1' of {
                  Eq ->
                   case compare27 t t' of {
                    Eq -> compare10 l2 l2';
                    x0 -> x0};
                  x0 -> x0}}}}} of {
     Eq -> Node0 h l y r;
     Lt -> bal0 (add8 x l) y r;
     Gt -> bal0 l y (add8 x r)}}

type T25 = Prod (Prod T3 T24) T3
make2 :: T3 -> T24 -> T3 -> T25
make2 l1 ct l2 =
  Pair (Pair l1 ct) l2

--make2 :: T_ -> Program -> T_ -> Program
--make2 a b c= Compose3 (WrapT a) (Wrap b) (WrapT c)

--le0 :: Program
--le0 = Le0
add_list :: (List T14) -> NonEmptyLevelExprSet -> NonEmptyLevelExprSet
add7 :: T14 -> NonEmptyLevelExprSet -> NonEmptyLevelExprSet
t_set :: NonEmptyLevelExprSet -> T21
t_set n =
  n

add7 e u =
  add6 e (t_set u)
add6 :: Elt2 -> T21 -> T21
add6 x s =
  add5 x (this0 s)
this0 :: T_1 -> T18
this0 t =
  t
make1 :: T14 -> T22
make1 =
  singleton3

add5 :: (Prod T3 Nat) -> (List (Prod T3 Nat)) -> List (Prod T3 Nat)
add5 x s =
  case s of {
   Nil -> Cons x Nil;
   Cons y l ->
    case case x of {
          Pair l1 b1 ->
           case y of {
            Pair l2 b2 ->
             case compare10 l1 l2 of {
              Eq -> my_compare b1 b2;
              x0 -> x0}}} of {
     Eq -> s;
     Lt -> Cons x s;
     Gt -> Cons y (add5 x l)}}

add_list =
  fold_left (\u e -> add7 e u)
fold_left :: (a1 -> a2 -> a1) -> (List a2) -> a1 -> a1
fold_left f l a0 =
  case l of {
   Nil -> a0;
   Cons b t -> fold_left f t (f a0 b)}

fst :: (Prod a1 a2) -> a1
fst p =
  case p of {
   Pair x _ -> x}

snd :: (Prod a1 a2) -> a2
snd p =
  case p of {
   Pair _ y -> y}

from_kernel_repr :: (Prod T3 Bool) -> T14
from_kernel_repr e =
  Pair (fst e) (case snd e of {
                 True -> S O;
                 False -> O})

my_map :: (a1 -> a2) -> (List a1) -> List a2
my_map f l =
  case l of {
   Nil -> Nil;
   Cons a t -> Cons (f a) (my_map f t)}

from_kernel_repr1 :: (Prod T3 Bool) -> (List (Prod T3 Bool)) -> T23
option_map :: (a1 -> a2) -> (Option a1) -> Option a2
option_map f o =
  case o of {
   Some a -> Some (f a);
   None -> None}

from_kernel_repr1 e es =
  LType (add_list (my_map from_kernel_repr es) (make1 (from_kernel_repr e)))

le0 :: T_3
le0 =
  Le Z0

--leo :: Program
--leo = Le0
--from_kernel_repr1 :: Program -> Program -> Program
--from_kernel_repr1 = Compose2
--lt :: Program
--lt = Lt
-- empty4 :: Program
-- empty4 = Empty4
empty4 :: T32
empty4 =
  empty3

data Tree0 =
   Leaf0
 | Node0 T0 Tree0 (Prod (Prod T3 T24) T3) Tree0
  deriving ( Generic, Show, GQLType, Data, Typeable)

empty3 :: Tree0

empty3 =
  Leaf0

lt :: T_3
lt =
  Le (Zpos XH)
data T13 =
   LSProp
 | LProp
  deriving ( Generic, Show, GQLType, Data, Typeable)
data Sum a b =
   Inl a
 | Inr b
  deriving ( Generic, Show, GQLType, Data, Typeable)
type T14 = Prod T3 Nat
singleton3 :: T14 -> NonEmptyLevelExprSet
singleton3 =
  singleton2
type Elt2 = Prod T3 Nat
singleton2 :: Elt2 -> T21
singleton2 =
  singleton1

singleton1 :: Elt1 -> List Elt1
singleton1 x =
  Cons x Nil
make :: T3 -> T14
make l =
  Pair l O

make' :: T3 -> NonEmptyLevelExprSet
make' l =
  singleton3 (make l)

of_levels :: (Sum T13 T3) -> T23
of_levels l =
  case l of {
   Inl t -> case t of {
             LSProp -> LSProp0;
             LProp -> LProp0};
   Inr l0 -> LType (make' l0)}

--of_levels = ofLevels 
--ofLevels :: Program -> Program
--ofLevels = OfLevels 

type Ident = MyString
type Dirpath = List Ident
data Modpath =
   MPfile Dirpath
 | MPbound Dirpath Ident Nat
 | MPdot Modpath Ident
 deriving ( Generic, Show, GQLType, Data, Typeable)

type Kername = Prod Modpath Ident
type T33 = List T3
type T3 = T_

-- poly morphic not used
-- type T34 = Prod (List Name) T32
type T_4 = T26
data T_3 =
   Le Z
 | Eq0
  deriving ( Generic, Show, GQLType, Data, Typeable)

type T24 = T_3

type T26 = Tree0
-- type T32 = T_4
-- type T35 = Prod T10 T32
data Name =
   NAnon
 | NNamed Ident
          deriving ( Generic, Show, GQLType, Data, Typeable)

data Relevance =
   Relevant
 | Irrelevant
  deriving ( Generic, Show, GQLType, Data, Typeable)


type Binder_annot  = BinderAnnot
data BinderAnnot a =
   MkBindAnn a Relevance
  deriving ( Generic, Show, GQLType, Data, Typeable)

type Aname = Binder_annot Name

type Cast_kind = CastKind
data CastKind =
   VmCast
 | NativeCast
 | Cast
  deriving ( Generic, Show, GQLType, Data, Typeable)

type Case_info = CaseInfo
data Inductive =
   MkInd Kername Nat
  deriving ( Generic, Show, GQLType, Data, Typeable)

data CaseInfo =
  Mk_case_info Inductive Nat Relevance
  deriving ( Generic, Show, GQLType, Data, Typeable)

type Recursivity_kind = RecursivityKind
data RecursivityKind =
   Finite
 | CoFinite
 | BiFinite
 deriving ( Generic, Show, GQLType, Data, Typeable)

data Def term =
   Mkdef Aname term term Nat
  deriving ( Generic, Show, GQLType, Data, Typeable)

type Universes_decl = UniversesDecl
data UniversesDecl =
   Monomorphic_ctx
--  | Polymorphic_ctx T34  not used 
  deriving ( Generic, Show, GQLType, Data, Typeable)

data Projection =
   MkProjection Inductive Nat Nat
  deriving ( Generic, Show, GQLType, Data, Typeable)

--type Term = Program
type Mfixpoint term = List (Def term)
data Term =
   TRel Nat
 | TVar Ident
 | TEvar Nat (List Term)
 | TSort T23
 | TCast Term Cast_kind Term
 | TProd Aname Term Term
 | TLambda Aname Term Term
 | TLetIn Aname Term Term Term
 | TApp Term (List Term)
 | TConst Kername T33
 | TInd Inductive T33
 | TConstruct Inductive Nat T33
 | TCase Case_info (Predicate Term) Term (List (Branch Term))
 | TProj Projection Term
 | TFix (Mfixpoint Term) Nat
 | TCoFix (Mfixpoint Term) Nat
 | TInt Prelude.Int
 | TFloat Prelude.Float
  deriving ( Generic, Show, GQLType, Data, Typeable)
data Constant_body =
   Build_constant_body Term (Option Term) Universes_decl Relevance
  deriving ( Generic, Show, GQLType, Data, Typeable)

data T36 =
   Irrelevant0
 | Covariant
 | Invariant
  deriving ( Generic, Show, GQLType, Data, Typeable)

data Option a =
   Some a
 | None
  deriving ( Generic, Show, GQLType, Data, Typeable)

data Context_decl term =
   Mkdecl Aname (Option term) term
  deriving ( Generic, Show, GQLType, Data, Typeable)

type Context = List (Context_decl Term)

type T22 = NonEmptyLevelExprSet
type Elt1 = Prod T3 Nat
type T18 = List Elt1
data Constructor_body =
   Build_constructor_body Ident Context (List Term) Term Nat
 deriving ( Generic, Show, GQLType, Data, Typeable)

type T_1 = T18
type T21 = T_1
type NonEmptyLevelExprSet =
   T21
    -- singleton inductive, whose constructor was Build_nonEmptyLevelExprSet

-- It would be nice to be able to automatically resolve some of these errors
type Mutual_inductive_body = MutualInductiveBody
data T_2 =
   LProp0
 | LSProp0
 | LType T22
 deriving ( Generic, Show, GQLType, Data, Typeable)

type T23 = T_2

data Allowed_eliminations =
   IntoSProp
 | IntoPropSProp
 | IntoSetPropSProp
 | IntoAny
 deriving ( Generic, Show, GQLType, Data, Typeable)

data Projection_body =
   Build_projection_body Ident Relevance Term
 deriving ( Generic, Show, GQLType, Data, Typeable)

data One_inductive_body =
   Build_one_inductive_body Ident Context T23 Term Allowed_eliminations 
 (List Constructor_body) (List Projection_body) Relevance
 deriving ( Generic, Show, GQLType, Data, Typeable)

data MutualInductiveBody =
   Build_mutual_inductive_body Recursivity_kind Nat Context (List
                                                            One_inductive_body) 
  Universes_decl (Option (List T36))
 deriving ( Generic, Show, GQLType, Data, Typeable)

data Global_decl =
    ConstantDecl Constant_body
  | InductiveDecl Mutual_inductive_body
 deriving ( Generic, Show, GQLType, Data, Typeable)
data Z =
   Z0
 | Zpos Positive
 | Zneg Positive
  deriving ( Generic, Show, GQLType, Data, Typeable)

data Tree =
   Leaf
 | Node T0 Tree T_ Tree
  deriving ( Generic, Show, GQLType, Data, Typeable)
type T4 = Tree
type T_0 = T4
type T10 = T_0
type Global_declarations = List (Prod Kername Global_decl)
type T32 = T_4
type T35 = Prod T10 T32
data T37 =
   Mk_retroknowledge (Option Kername) (Option Kername)
  deriving ( Generic, Show, GQLType, Data, Typeable)

data Global_env =
  -- Global_declarations
  Mk_global_env T35 Global_declarations T37
  -- Mk_global_env Program Program Program
--  | Mk_global_env (Prod Program Program) (List Program) Program
                deriving ( Generic, Show, GQLType, Data, Typeable)
data Prod a b =
    Pair a b
 deriving ( Generic, Show, GQLType, Data, Typeable)

--data Body = 
--  Build_mutual_inductive_body Program Nat  ListProgram  Program Program Program

-- data Program = Program
--   | Foo
--              | MFalse 
-- --             | BiFinite 
-- --             | Build_constant_body Program Program Program Program 
-- --             | Build_constructor_body Program Program Program Program Nat

-- --             | Build_one_inductive_body Program  Program Program Program Program  Program Program Program 
--              | Compose2 Program Program 
--              | Compose3 Program Program Program
-- --             | Cons MyString Program
--              -- | ConstantDecl Program 
--              | Empty4

-- --             | Eq0
-- --             | Finite
-- --             | InductiveDecl Program 
-- --             | Inl Program 
-- --             | Inr Program 
-- --             | IntoAny
-- --             | IntoPropSProp
-- --             | LProp
-- --             | Le Program
--              | Le0

-- --             | Lt
-- --             | Lvar Program



-- --             | MkBindAnn  Program Program
-- --             | MkInd  Program Nat
--              | Mk_branch  Program Program 
-- --             | Mk_case_info Program Nat Program 
data Branch term =
   Mk_branch (List Aname) term
  deriving ( Generic, Show, GQLType, Data, Typeable)


--              | Mk_predicate Program Program  Program Program
data Predicate term =
   Mk_predicate T33 (List term) (List Aname) term
  deriving ( Generic, Show, GQLType, Data, Typeable)
-- --             | Mk_retroknowledge Program Program
-- --             | Mkdecl   Program  Program  Program
-- --             | Mkdef Program Program Program  Nat
-- --             | Monomorphic_ctx
-- --             | NAnon 
-- --             | NNamed Program 
-- --             | Nil
-- --             | None
-- --             | O
--              | OfLevels Program 
--              | OfList 
-- --             | Pair Program Program
-- --             | Relevant
-- --             | S Program 
-- --             | Some Program 
--              | TApp  Program  Program
--              | TCase Program Program Program Program
--              | TConst Program Program
--              | TConstruct Program Nat Program
--              | TFix Program Nat
--              | TInd Program Program 
--              | TLambda Program Program Program
--              | TProd Program Program Program 
--              | TRel Nat
--              | TSort Program 
--              | Wrap Program
--              | WrapT T_

--              deriving ( Generic, Show, GQLType, Data, Typeable)



data List a =
   Nil
 | Cons a (List a)
            deriving ( Generic, Show, GQLType, Data, Typeable)
-- deriving ( Generic, Show, GQLType, Data, Typeable)

--concatChars :: T -> Prelude.String
concatChars :: MyString -> [Char]
concatChars = \case {
  EmptyString -> "";
    String byte rest -> toString byte : concatChars rest
  }



data Byte =
   X00
 | X01
 | X02
 | X03
 | X04
 | X05
 | X06
 | X07
 | X08
 | X09
 | X0a
 | X0b
 | X0c
 | X0d
 | X0e
 | X0f
 | X10
 | X11
 | X12
 | X13
 | X14
 | X15
 | X16
 | X17
 | X18
 | X19
 | X1a
 | X1b
 | X1c
 | X1d
 | X1e
 | X1f
 | X20
 | X21
 | X22
 | X23
 | X24
 | X25
 | X26
 | X27
 | X28
 | X29
 | X2a
 | X2b
 | X2c
 | X2d
 | X2e
 | X2f
 | X30
 | X31
 | X32
 | X33
 | X34
 | X35
 | X36
 | X37
 | X38
 | X39
 | X3a
 | X3b
 | X3c
 | X3d
 | X3e
 | X3f
 | X40
 | X41
 | X42
 | X43
 | X44
 | X45
 | X46
 | X47
 | X48
 | X49
 | X4a
 | X4b
 | X4c
 | X4d
 | X4e
 | X4f
 | X50
 | X51
 | X52
 | X53
 | X54
 | X55
 | X56
 | X57
 | X58
 | X59
 | X5a
 | X5b
 | X5c
 | X5d
 | X5e
 | X5f
 | X60
 | X61
 | X62
 | X63
 | X64
 | X65
 | X66
 | X67
 | X68
 | X69
 | X6a
 | X6b
 | X6c
 | X6d
 | X6e
 | X6f
 | X70
 | X71
 | X72
 | X73
 | X74
 | X75
 | X76
 | X77
 | X78
 | X79
 | X7a
 | X7b
 | X7c
 | X7d
 | X7e
 | X7f
 | X80
 | X81
 | X82
 | X83
 | X84
 | X85
 | X86
 | X87
 | X88
 | X89
 | X8a
 | X8b
 | X8c
 | X8d
 | X8e
 | X8f
 | X90
 | X91
 | X92
 | X93
 | X94
 | X95
 | X96
 | X97
 | X98
 | X99
 | X9a
 | X9b
 | X9c
 | X9d
 | X9e
 | X9f
 | Xa0
 | Xa1
 | Xa2
 | Xa3
 | Xa4
 | Xa5
 | Xa6
 | Xa7
 | Xa8
 | Xa9
 | Xaa
 | Xab
 | Xac
 | Xad
 | Xae
 | Xaf
 | Xb0
 | Xb1
 | Xb2
 | Xb3
 | Xb4
 | Xb5
 | Xb6
 | Xb7
 | Xb8
 | Xb9
 | Xba
 | Xbb
 | Xbc
 | Xbd
 | Xbe
 | Xbf
 | Xc0
 | Xc1
 | Xc2
 | Xc3
 | Xc4
 | Xc5
 | Xc6
 | Xc7
 | Xc8
 | Xc9
 | Xca
 | Xcb
 | Xcc
 | Xcd
 | Xce
 | Xcf
 | Xd0
 | Xd1
 | Xd2
 | Xd3
 | Xd4
 | Xd5
 | Xd6
 | Xd7
 | Xd8
 | Xd9
 | Xda
 | Xdb
 | Xdc
 | Xdd
 | Xde
 | Xdf
 | Xe0
 | Xe1
 | Xe2
 | Xe3
 | Xe4
 | Xe5
 | Xe6
 | Xe7
 | Xe8
 | Xe9
 | Xea
 | Xeb
 | Xec
 | Xed
 | Xee
 | Xef
 | Xf0
 | Xf1
 | Xf2
 | Xf3
 | Xf4
 | Xf5
 | Xf6
 | Xf7
 | Xf8
 | Xf9
 | Xfa
 | Xfb
 | Xfc
 | Xfd
 | Xfe
 | Xff
 deriving (Show, GQLType,  Generic, Data, Typeable)

--instance GTraversable c Byte -- where
--  gtraverse f x = 
  --  ofoldMap f con = f (pack (show con))
    --foldr (mappend . f) mempty

toN :: Byte -> N
toN x =
  case x of {
   X00 -> n0;
   X01 -> n1;
   X02 -> n2;
   X03 -> n3;
   X04 -> n4;
   X05 -> n5;
   X06 -> n6;
   X07 -> n7;
   X08 -> n8;
   X09 -> n9;
   X0a -> n10;
   X0b -> n11;
   X0c -> n12;
   X0d -> n13;
   X0e -> n14;
   X0f -> n15;
   X10 -> n16;
   X11 -> n17;
   X12 -> n18;
   X13 -> n19;
   X14 -> n20;
   X15 -> n21;
   X16 -> n22;
   X17 -> n23;
   X18 -> n24;
   X19 -> n25;
   X1a -> n26;
   X1b -> n27;
   X1c -> n28;
   X1d -> n29;
   X1e -> n30;
   X1f -> n31;
   X20 -> n32;
   X21 -> n33;
   X22 -> n34;
   X23 -> n35;
   X24 -> n36;
   X25 -> n37;
   X26 -> n38;
   X27 -> n39;
   X28 -> n40;
   X29 -> n41;
   X2a -> n42;
   X2b -> n43;
   X2c -> n44;
   X2d -> n45;
   X2e -> n46;
   X2f -> n47;
   X30 -> n48;
   X31 -> n49;
   X32 -> n50;
   X33 -> n51;
   X34 -> n52;
   X35 -> n53;
   X36 -> n54;
   X37 -> n55;
   X38 -> n56;
   X39 -> n57;
   X3a -> n58;
   X3b -> n59;
   X3c -> n60;
   X3d -> n61;
   X3e -> n62;
   X3f -> n63;
   X40 -> n64;
   X41 -> n65;
   X42 -> n66;
   X43 -> n67;
   X44 -> n68;
   X45 -> n69;
   X46 -> n70;
   X47 -> n71;
   X48 -> n72;
   X49 -> n73;
   X4a -> n74;
   X4b -> n75;
   X4c -> n76;
   X4d -> n77;
   X4e -> n78;
   X4f -> n79;
   X50 -> n80;
   X51 -> n81;
   X52 -> n82;
   X53 -> n83;
   X54 -> n84;
   X55 -> n85;
   X56 -> n86;
   X57 -> n87;
   X58 -> n88;
   X59 -> n89;
   X5a -> n90;
   X5b -> n91;
   X5c -> n92;
   X5d -> n93;
   X5e -> n94;
   X5f -> n95;
   X60 -> n96;
   X61 -> n97;
   X62 -> n98;
   X63 -> n99;
   X64 -> n100;
   X65 -> n101;
   X66 -> n102;
   X67 -> n103;
   X68 -> n104;
   X69 -> n105;
   X6a -> n106;
   X6b -> n107;
   X6c -> n108;
   X6d -> n109;
   X6e -> n110;
   X6f -> n111;
   X70 -> n112;
   X71 -> n113;
   X72 -> n114;
   X73 -> n115;
   X74 -> n116;
   X75 -> n117;
   X76 -> n118;
   X77 -> n119;
   X78 -> n120;
   X79 -> n121;
   X7a -> n122;
   X7b -> n123;
   X7c -> n124;
   X7d -> n125;
   X7e -> n126;
   X7f -> n127;
   X80 -> n128;
   X81 -> n129;
   X82 -> n130;
   X83 -> n131;
   X84 -> n132;
   X85 -> n133;
   X86 -> n134;
   X87 -> n135;
   X88 -> n136;
   X89 -> n137;
   X8a -> n138;
   X8b -> n139;
   X8c -> n140;
   X8d -> n141;
   X8e -> n142;
   X8f -> n143;
   X90 -> n144;
   X91 -> n145;
   X92 -> n146;
   X93 -> n147;
   X94 -> n148;
   X95 -> n149;
   X96 -> n150;
   X97 -> n151;
   X98 -> n152;
   X99 -> n153;
   X9a -> n154;
   X9b -> n155;
   X9c -> n156;
   X9d -> n157;
   X9e -> n158;
   X9f -> n159;
   Xa0 -> n160;
   Xa1 -> n161;
   Xa2 -> n162;
   Xa3 -> n163;
   Xa4 -> n164;
   Xa5 -> n165;
   Xa6 -> n166;
   Xa7 -> n167;
   Xa8 -> n168;
   Xa9 -> n169;
   Xaa -> n170;
   Xab -> n171;
   Xac -> n172;
   Xad -> n173;
   Xae -> n174;
   Xaf -> n175;
   Xb0 -> n176;
   Xb1 -> n177;
   Xb2 -> n178;
   Xb3 -> n179;
   Xb4 -> n180;
   Xb5 -> n181;
   Xb6 -> n182;
   Xb7 -> n183;
   Xb8 -> n184;
   Xb9 -> n185;
   Xba -> n186;
   Xbb -> n187;
   Xbc -> n188;
   Xbd -> n189;
   Xbe -> n190;
   Xbf -> n191;
   Xc0 -> n192;
   Xc1 -> n193;
   Xc2 -> n194;
   Xc3 -> n195;
   Xc4 -> n196;
   Xc5 -> n197;
   Xc6 -> n198;
   Xc7 -> n199;
   Xc8 -> n200;
   Xc9 -> n201;
   Xca -> n202;
   Xcb -> n203;
   Xcc -> n204;
   Xcd -> n205;
   Xce -> n206;
   Xcf -> n207;
   Xd0 -> n208;
   Xd1 -> n209;
   Xd2 -> n210;
   Xd3 -> n211;
   Xd4 -> n212;
   Xd5 -> n213;
   Xd6 -> n214;
   Xd7 -> n215;
   Xd8 -> n216;
   Xd9 -> n217;
   Xda -> n218;
   Xdb -> n219;
   Xdc -> n220;
   Xdd -> n221;
   Xde -> n222;
   Xdf -> n223;
   Xe0 -> n224;
   Xe1 -> n225;
   Xe2 -> n226;
   Xe3 -> n227;
   Xe4 -> n228;
   Xe5 -> n229;
   Xe6 -> n230;
   Xe7 -> n231;
   Xe8 -> n232;
   Xe9 -> n233;
   Xea -> n234;
   Xeb -> n235;
   Xec -> n236;
   Xed -> n237;
   Xee -> n238;
   Xef -> n239;
   Xf0 -> n240;
   Xf1 -> n241;
   Xf2 -> n242;
   Xf3 -> n243;
   Xf4 -> n244;
   Xf5 -> n245;
   Xf6 -> n246;
   Xf7 -> n247;
   Xf8 -> n248;
   Xf9 -> n249;
   Xfa -> n250;
   Xfb -> n251;
   Xfc -> n252;
   Xfd -> n253;
   Xfe -> n254;
   Xff -> n255}

n0 :: N
n0 =
  N0

n1 :: N
n1 =
  Npos XH

n2 :: N
n2 =
  Npos (XO XH)

n3 :: N
n3 =
  Npos (XI XH)

n4 :: N
n4 =
  Npos (XO (XO XH))

n5 :: N
n5 =
  Npos (XI (XO XH))

n6 :: N
n6 =
  Npos (XO (XI XH))

n7 :: N
n7 =
  Npos (XI (XI XH))

n8 :: N
n8 =
  Npos (XO (XO (XO XH)))

n9 :: N
n9 =
  Npos (XI (XO (XO XH)))

n10 :: N
n10 =
  Npos (XO (XI (XO XH)))

n11 :: N
n11 =
  Npos (XI (XI (XO XH)))

n12 :: N
n12 =
  Npos (XO (XO (XI XH)))

n13 :: N
n13 =
  Npos (XI (XO (XI XH)))

n14 :: N
n14 =
  Npos (XO (XI (XI XH)))

n15 :: N
n15 =
  Npos (XI (XI (XI XH)))

n16 :: N
n16 =
  Npos (XO (XO (XO (XO XH))))

n17 :: N
n17 =
  Npos (XI (XO (XO (XO XH))))

n18 :: N
n18 =
  Npos (XO (XI (XO (XO XH))))

n19 :: N
n19 =
  Npos (XI (XI (XO (XO XH))))

n20 :: N
n20 =
  Npos (XO (XO (XI (XO XH))))

n21 :: N
n21 =
  Npos (XI (XO (XI (XO XH))))

n22 :: N
n22 =
  Npos (XO (XI (XI (XO XH))))

n23 :: N
n23 =
  Npos (XI (XI (XI (XO XH))))

n24 :: N
n24 =
  Npos (XO (XO (XO (XI XH))))

n25 :: N
n25 =
  Npos (XI (XO (XO (XI XH))))

n26 :: N
n26 =
  Npos (XO (XI (XO (XI XH))))

n27 :: N
n27 =
  Npos (XI (XI (XO (XI XH))))

n28 :: N
n28 =
  Npos (XO (XO (XI (XI XH))))

n29 :: N
n29 =
  Npos (XI (XO (XI (XI XH))))

n30 :: N
n30 =
  Npos (XO (XI (XI (XI XH))))

n31 :: N
n31 =
  Npos (XI (XI (XI (XI XH))))

n32 :: N
n32 =
  Npos (XO (XO (XO (XO (XO XH)))))

n33 :: N
n33 =
  Npos (XI (XO (XO (XO (XO XH)))))

n34 :: N
n34 =
  Npos (XO (XI (XO (XO (XO XH)))))

n35 :: N
n35 =
  Npos (XI (XI (XO (XO (XO XH)))))

n36 :: N
n36 =
  Npos (XO (XO (XI (XO (XO XH)))))

n37 :: N
n37 =
  Npos (XI (XO (XI (XO (XO XH)))))

n38 :: N
n38 =
  Npos (XO (XI (XI (XO (XO XH)))))

n39 :: N
n39 =
  Npos (XI (XI (XI (XO (XO XH)))))

n40 :: N
n40 =
  Npos (XO (XO (XO (XI (XO XH)))))

n41 :: N
n41 =
  Npos (XI (XO (XO (XI (XO XH)))))

n42 :: N
n42 =
  Npos (XO (XI (XO (XI (XO XH)))))

n43 :: N
n43 =
  Npos (XI (XI (XO (XI (XO XH)))))

n44 :: N
n44 =
  Npos (XO (XO (XI (XI (XO XH)))))

n45 :: N
n45 =
  Npos (XI (XO (XI (XI (XO XH)))))

n46 :: N
n46 =
  Npos (XO (XI (XI (XI (XO XH)))))

n47 :: N
n47 =
  Npos (XI (XI (XI (XI (XO XH)))))

n48 :: N
n48 =
  Npos (XO (XO (XO (XO (XI XH)))))

n49 :: N
n49 =
  Npos (XI (XO (XO (XO (XI XH)))))

n50 :: N
n50 =
  Npos (XO (XI (XO (XO (XI XH)))))

n51 :: N
n51 =
  Npos (XI (XI (XO (XO (XI XH)))))

n52 :: N
n52 =
  Npos (XO (XO (XI (XO (XI XH)))))

n53 :: N
n53 =
  Npos (XI (XO (XI (XO (XI XH)))))

n54 :: N
n54 =
  Npos (XO (XI (XI (XO (XI XH)))))

n55 :: N
n55 =
  Npos (XI (XI (XI (XO (XI XH)))))

n56 :: N
n56 =
  Npos (XO (XO (XO (XI (XI XH)))))

n57 :: N
n57 =
  Npos (XI (XO (XO (XI (XI XH)))))

n58 :: N
n58 =
  Npos (XO (XI (XO (XI (XI XH)))))

n59 :: N
n59 =
  Npos (XI (XI (XO (XI (XI XH)))))

n60 :: N
n60 =
  Npos (XO (XO (XI (XI (XI XH)))))

n61 :: N
n61 =
  Npos (XI (XO (XI (XI (XI XH)))))

n62 :: N
n62 =
  Npos (XO (XI (XI (XI (XI XH)))))

n63 :: N
n63 =
  Npos (XI (XI (XI (XI (XI XH)))))

n64 :: N
n64 =
  Npos (XO (XO (XO (XO (XO (XO XH))))))

n65 :: N
n65 =
  Npos (XI (XO (XO (XO (XO (XO XH))))))

n66 :: N
n66 =
  Npos (XO (XI (XO (XO (XO (XO XH))))))

n67 :: N
n67 =
  Npos (XI (XI (XO (XO (XO (XO XH))))))

n68 :: N
n68 =
  Npos (XO (XO (XI (XO (XO (XO XH))))))

n69 :: N
n69 =
  Npos (XI (XO (XI (XO (XO (XO XH))))))

n70 :: N
n70 =
  Npos (XO (XI (XI (XO (XO (XO XH))))))

n71 :: N
n71 =
  Npos (XI (XI (XI (XO (XO (XO XH))))))

n72 :: N
n72 =
  Npos (XO (XO (XO (XI (XO (XO XH))))))

n73 :: N
n73 =
  Npos (XI (XO (XO (XI (XO (XO XH))))))

n74 :: N
n74 =
  Npos (XO (XI (XO (XI (XO (XO XH))))))

n75 :: N
n75 =
  Npos (XI (XI (XO (XI (XO (XO XH))))))

n76 :: N
n76 =
  Npos (XO (XO (XI (XI (XO (XO XH))))))

n77 :: N
n77 =
  Npos (XI (XO (XI (XI (XO (XO XH))))))

n78 :: N
n78 =
  Npos (XO (XI (XI (XI (XO (XO XH))))))

n79 :: N
n79 =
  Npos (XI (XI (XI (XI (XO (XO XH))))))

n80 :: N
n80 =
  Npos (XO (XO (XO (XO (XI (XO XH))))))

n81 :: N
n81 =
  Npos (XI (XO (XO (XO (XI (XO XH))))))

n82 :: N
n82 =
  Npos (XO (XI (XO (XO (XI (XO XH))))))

n83 :: N
n83 =
  Npos (XI (XI (XO (XO (XI (XO XH))))))

n84 :: N
n84 =
  Npos (XO (XO (XI (XO (XI (XO XH))))))

n85 :: N
n85 =
  Npos (XI (XO (XI (XO (XI (XO XH))))))

n86 :: N
n86 =
  Npos (XO (XI (XI (XO (XI (XO XH))))))

n87 :: N
n87 =
  Npos (XI (XI (XI (XO (XI (XO XH))))))

n88 :: N
n88 =
  Npos (XO (XO (XO (XI (XI (XO XH))))))

n89 :: N
n89 =
  Npos (XI (XO (XO (XI (XI (XO XH))))))

n90 :: N
n90 =
  Npos (XO (XI (XO (XI (XI (XO XH))))))

n91 :: N
n91 =
  Npos (XI (XI (XO (XI (XI (XO XH))))))

n92 :: N
n92 =
  Npos (XO (XO (XI (XI (XI (XO XH))))))

n93 :: N
n93 =
  Npos (XI (XO (XI (XI (XI (XO XH))))))

n94 :: N
n94 =
  Npos (XO (XI (XI (XI (XI (XO XH))))))

n95 :: N
n95 =
  Npos (XI (XI (XI (XI (XI (XO XH))))))

n96 :: N
n96 =
  Npos (XO (XO (XO (XO (XO (XI XH))))))

n97 :: N
n97 =
  Npos (XI (XO (XO (XO (XO (XI XH))))))

n98 :: N
n98 =
  Npos (XO (XI (XO (XO (XO (XI XH))))))

n99 :: N
n99 =
  Npos (XI (XI (XO (XO (XO (XI XH))))))

n100 :: N
n100 =
  Npos (XO (XO (XI (XO (XO (XI XH))))))

n101 :: N
n101 =
  Npos (XI (XO (XI (XO (XO (XI XH))))))

n102 :: N
n102 =
  Npos (XO (XI (XI (XO (XO (XI XH))))))

n103 :: N
n103 =
  Npos (XI (XI (XI (XO (XO (XI XH))))))

n104 :: N
n104 =
  Npos (XO (XO (XO (XI (XO (XI XH))))))

n105 :: N
n105 =
  Npos (XI (XO (XO (XI (XO (XI XH))))))

n106 :: N
n106 =
  Npos (XO (XI (XO (XI (XO (XI XH))))))

n107 :: N
n107 =
  Npos (XI (XI (XO (XI (XO (XI XH))))))

n108 :: N
n108 =
  Npos (XO (XO (XI (XI (XO (XI XH))))))

n109 :: N
n109 =
  Npos (XI (XO (XI (XI (XO (XI XH))))))

n110 :: N
n110 =
  Npos (XO (XI (XI (XI (XO (XI XH))))))

n111 :: N
n111 =
  Npos (XI (XI (XI (XI (XO (XI XH))))))

n112 :: N
n112 =
  Npos (XO (XO (XO (XO (XI (XI XH))))))

n113 :: N
n113 =
  Npos (XI (XO (XO (XO (XI (XI XH))))))

n114 :: N
n114 =
  Npos (XO (XI (XO (XO (XI (XI XH))))))

n115 :: N
n115 =
  Npos (XI (XI (XO (XO (XI (XI XH))))))

n116 :: N
n116 =
  Npos (XO (XO (XI (XO (XI (XI XH))))))

n117 :: N
n117 =
  Npos (XI (XO (XI (XO (XI (XI XH))))))

n118 :: N
n118 =
  Npos (XO (XI (XI (XO (XI (XI XH))))))

n119 :: N
n119 =
  Npos (XI (XI (XI (XO (XI (XI XH))))))

n120 :: N
n120 =
  Npos (XO (XO (XO (XI (XI (XI XH))))))

n121 :: N
n121 =
  Npos (XI (XO (XO (XI (XI (XI XH))))))

n122 :: N
n122 =
  Npos (XO (XI (XO (XI (XI (XI XH))))))

n123 :: N
n123 =
  Npos (XI (XI (XO (XI (XI (XI XH))))))

n124 :: N
n124 =
  Npos (XO (XO (XI (XI (XI (XI XH))))))

n125 :: N
n125 =
  Npos (XI (XO (XI (XI (XI (XI XH))))))

n126 :: N
n126 =
  Npos (XO (XI (XI (XI (XI (XI XH))))))

n127 :: N
n127 =
  Npos (XI (XI (XI (XI (XI (XI XH))))))

n128 :: N
n128 =
  Npos (XO (XO (XO (XO (XO (XO (XO XH)))))))

n129 :: N
n129 =
  Npos (XI (XO (XO (XO (XO (XO (XO XH)))))))

n130 :: N
n130 =
  Npos (XO (XI (XO (XO (XO (XO (XO XH)))))))

n131 :: N
n131 =
  Npos (XI (XI (XO (XO (XO (XO (XO XH)))))))

n132 :: N
n132 =
  Npos (XO (XO (XI (XO (XO (XO (XO XH)))))))

n133 :: N
n133 =
  Npos (XI (XO (XI (XO (XO (XO (XO XH)))))))

n134 :: N
n134 =
  Npos (XO (XI (XI (XO (XO (XO (XO XH)))))))

n135 :: N
n135 =
  Npos (XI (XI (XI (XO (XO (XO (XO XH)))))))

n136 :: N
n136 =
  Npos (XO (XO (XO (XI (XO (XO (XO XH)))))))

n137 :: N
n137 =
  Npos (XI (XO (XO (XI (XO (XO (XO XH)))))))

n138 :: N
n138 =
  Npos (XO (XI (XO (XI (XO (XO (XO XH)))))))

n139 :: N
n139 =
  Npos (XI (XI (XO (XI (XO (XO (XO XH)))))))

n140 :: N
n140 =
  Npos (XO (XO (XI (XI (XO (XO (XO XH)))))))

n141 :: N
n141 =
  Npos (XI (XO (XI (XI (XO (XO (XO XH)))))))

n142 :: N
n142 =
  Npos (XO (XI (XI (XI (XO (XO (XO XH)))))))

n143 :: N
n143 =
  Npos (XI (XI (XI (XI (XO (XO (XO XH)))))))

n144 :: N
n144 =
  Npos (XO (XO (XO (XO (XI (XO (XO XH)))))))

n145 :: N
n145 =
  Npos (XI (XO (XO (XO (XI (XO (XO XH)))))))

n146 :: N
n146 =
  Npos (XO (XI (XO (XO (XI (XO (XO XH)))))))

n147 :: N
n147 =
  Npos (XI (XI (XO (XO (XI (XO (XO XH)))))))

n148 :: N
n148 =
  Npos (XO (XO (XI (XO (XI (XO (XO XH)))))))

n149 :: N
n149 =
  Npos (XI (XO (XI (XO (XI (XO (XO XH)))))))

n150 :: N
n150 =
  Npos (XO (XI (XI (XO (XI (XO (XO XH)))))))

n151 :: N
n151 =
  Npos (XI (XI (XI (XO (XI (XO (XO XH)))))))

n152 :: N
n152 =
  Npos (XO (XO (XO (XI (XI (XO (XO XH)))))))

n153 :: N
n153 =
  Npos (XI (XO (XO (XI (XI (XO (XO XH)))))))

n154 :: N
n154 =
  Npos (XO (XI (XO (XI (XI (XO (XO XH)))))))

n155 :: N
n155 =
  Npos (XI (XI (XO (XI (XI (XO (XO XH)))))))

n156 :: N
n156 =
  Npos (XO (XO (XI (XI (XI (XO (XO XH)))))))

n157 :: N
n157 =
  Npos (XI (XO (XI (XI (XI (XO (XO XH)))))))

n158 :: N
n158 =
  Npos (XO (XI (XI (XI (XI (XO (XO XH)))))))

n159 :: N
n159 =
  Npos (XI (XI (XI (XI (XI (XO (XO XH)))))))

n160 :: N
n160 =
  Npos (XO (XO (XO (XO (XO (XI (XO XH)))))))

n161 :: N
n161 =
  Npos (XI (XO (XO (XO (XO (XI (XO XH)))))))

n162 :: N
n162 =
  Npos (XO (XI (XO (XO (XO (XI (XO XH)))))))

n163 :: N
n163 =
  Npos (XI (XI (XO (XO (XO (XI (XO XH)))))))

n164 :: N
n164 =
  Npos (XO (XO (XI (XO (XO (XI (XO XH)))))))

n165 :: N
n165 =
  Npos (XI (XO (XI (XO (XO (XI (XO XH)))))))

n166 :: N
n166 =
  Npos (XO (XI (XI (XO (XO (XI (XO XH)))))))

n167 :: N
n167 =
  Npos (XI (XI (XI (XO (XO (XI (XO XH)))))))

n168 :: N
n168 =
  Npos (XO (XO (XO (XI (XO (XI (XO XH)))))))

n169 :: N
n169 =
  Npos (XI (XO (XO (XI (XO (XI (XO XH)))))))

n170 :: N
n170 =
  Npos (XO (XI (XO (XI (XO (XI (XO XH)))))))

n171 :: N
n171 =
  Npos (XI (XI (XO (XI (XO (XI (XO XH)))))))

n172 :: N
n172 =
  Npos (XO (XO (XI (XI (XO (XI (XO XH)))))))

n173 :: N
n173 =
  Npos (XI (XO (XI (XI (XO (XI (XO XH)))))))

n174 :: N
n174 =
  Npos (XO (XI (XI (XI (XO (XI (XO XH)))))))

n175 :: N
n175 =
  Npos (XI (XI (XI (XI (XO (XI (XO XH)))))))

n176 :: N
n176 =
  Npos (XO (XO (XO (XO (XI (XI (XO XH)))))))

n177 :: N
n177 =
  Npos (XI (XO (XO (XO (XI (XI (XO XH)))))))

n178 :: N
n178 =
  Npos (XO (XI (XO (XO (XI (XI (XO XH)))))))

n179 :: N
n179 =
  Npos (XI (XI (XO (XO (XI (XI (XO XH)))))))

n180 :: N
n180 =
  Npos (XO (XO (XI (XO (XI (XI (XO XH)))))))

n181 :: N
n181 =
  Npos (XI (XO (XI (XO (XI (XI (XO XH)))))))

n182 :: N
n182 =
  Npos (XO (XI (XI (XO (XI (XI (XO XH)))))))

n183 :: N
n183 =
  Npos (XI (XI (XI (XO (XI (XI (XO XH)))))))

n184 :: N
n184 =
  Npos (XO (XO (XO (XI (XI (XI (XO XH)))))))

n185 :: N
n185 =
  Npos (XI (XO (XO (XI (XI (XI (XO XH)))))))

n186 :: N
n186 =
  Npos (XO (XI (XO (XI (XI (XI (XO XH)))))))

n187 :: N
n187 =
  Npos (XI (XI (XO (XI (XI (XI (XO XH)))))))

n188 :: N
n188 =
  Npos (XO (XO (XI (XI (XI (XI (XO XH)))))))

n189 :: N
n189 =
  Npos (XI (XO (XI (XI (XI (XI (XO XH)))))))

n190 :: N
n190 =
  Npos (XO (XI (XI (XI (XI (XI (XO XH)))))))

n191 :: N
n191 =
  Npos (XI (XI (XI (XI (XI (XI (XO XH)))))))

n192 :: N
n192 =
  Npos (XO (XO (XO (XO (XO (XO (XI XH)))))))

n193 :: N
n193 =
  Npos (XI (XO (XO (XO (XO (XO (XI XH)))))))

n194 :: N
n194 =
  Npos (XO (XI (XO (XO (XO (XO (XI XH)))))))

n195 :: N
n195 =
  Npos (XI (XI (XO (XO (XO (XO (XI XH)))))))

n196 :: N
n196 =
  Npos (XO (XO (XI (XO (XO (XO (XI XH)))))))

n197 :: N
n197 =
  Npos (XI (XO (XI (XO (XO (XO (XI XH)))))))

n198 :: N
n198 =
  Npos (XO (XI (XI (XO (XO (XO (XI XH)))))))

n199 :: N
n199 =
  Npos (XI (XI (XI (XO (XO (XO (XI XH)))))))

n200 :: N
n200 =
  Npos (XO (XO (XO (XI (XO (XO (XI XH)))))))

n201 :: N
n201 =
  Npos (XI (XO (XO (XI (XO (XO (XI XH)))))))

n202 :: N
n202 =
  Npos (XO (XI (XO (XI (XO (XO (XI XH)))))))

n203 :: N
n203 =
  Npos (XI (XI (XO (XI (XO (XO (XI XH)))))))

n204 :: N
n204 =
  Npos (XO (XO (XI (XI (XO (XO (XI XH)))))))

n205 :: N
n205 =
  Npos (XI (XO (XI (XI (XO (XO (XI XH)))))))

n206 :: N
n206 =
  Npos (XO (XI (XI (XI (XO (XO (XI XH)))))))

n207 :: N
n207 =
  Npos (XI (XI (XI (XI (XO (XO (XI XH)))))))

n208 :: N
n208 =
  Npos (XO (XO (XO (XO (XI (XO (XI XH)))))))

n209 :: N
n209 =
  Npos (XI (XO (XO (XO (XI (XO (XI XH)))))))

n210 :: N
n210 =
  Npos (XO (XI (XO (XO (XI (XO (XI XH)))))))

n211 :: N
n211 =
  Npos (XI (XI (XO (XO (XI (XO (XI XH)))))))

n212 :: N
n212 =
  Npos (XO (XO (XI (XO (XI (XO (XI XH)))))))

n213 :: N
n213 =
  Npos (XI (XO (XI (XO (XI (XO (XI XH)))))))

n214 :: N
n214 =
  Npos (XO (XI (XI (XO (XI (XO (XI XH)))))))

n215 :: N
n215 =
  Npos (XI (XI (XI (XO (XI (XO (XI XH)))))))

n216 :: N
n216 =
  Npos (XO (XO (XO (XI (XI (XO (XI XH)))))))

n217 :: N
n217 =
  Npos (XI (XO (XO (XI (XI (XO (XI XH)))))))

n218 :: N
n218 =
  Npos (XO (XI (XO (XI (XI (XO (XI XH)))))))

n219 :: N
n219 =
  Npos (XI (XI (XO (XI (XI (XO (XI XH)))))))

n220 :: N
n220 =
  Npos (XO (XO (XI (XI (XI (XO (XI XH)))))))

n221 :: N
n221 =
  Npos (XI (XO (XI (XI (XI (XO (XI XH)))))))

n222 :: N
n222 =
  Npos (XO (XI (XI (XI (XI (XO (XI XH)))))))

n223 :: N
n223 =
  Npos (XI (XI (XI (XI (XI (XO (XI XH)))))))

n224 :: N
n224 =
  Npos (XO (XO (XO (XO (XO (XI (XI XH)))))))

n225 :: N
n225 =
  Npos (XI (XO (XO (XO (XO (XI (XI XH)))))))

n226 :: N
n226 =
  Npos (XO (XI (XO (XO (XO (XI (XI XH)))))))

n227 :: N
n227 =
  Npos (XI (XI (XO (XO (XO (XI (XI XH)))))))

n228 :: N
n228 =
  Npos (XO (XO (XI (XO (XO (XI (XI XH)))))))

n229 :: N
n229 =
  Npos (XI (XO (XI (XO (XO (XI (XI XH)))))))

n230 :: N
n230 =
  Npos (XO (XI (XI (XO (XO (XI (XI XH)))))))

n231 :: N
n231 =
  Npos (XI (XI (XI (XO (XO (XI (XI XH)))))))

n232 :: N
n232 =
  Npos (XO (XO (XO (XI (XO (XI (XI XH)))))))

n233 :: N
n233 =
  Npos (XI (XO (XO (XI (XO (XI (XI XH)))))))

n234 :: N
n234 =
  Npos (XO (XI (XO (XI (XO (XI (XI XH)))))))

n235 :: N
n235 =
  Npos (XI (XI (XO (XI (XO (XI (XI XH)))))))

n236 :: N
n236 =
  Npos (XO (XO (XI (XI (XO (XI (XI XH)))))))

n237 :: N
n237 =
  Npos (XI (XO (XI (XI (XO (XI (XI XH)))))))

n238 :: N
n238 =
  Npos (XO (XI (XI (XI (XO (XI (XI XH)))))))

n239 :: N
n239 =
  Npos (XI (XI (XI (XI (XO (XI (XI XH)))))))

n240 :: N
n240 =
  Npos (XO (XO (XO (XO (XI (XI (XI XH)))))))

n241 :: N
n241 =
  Npos (XI (XO (XO (XO (XI (XI (XI XH)))))))

n242 :: N
n242 =
  Npos (XO (XI (XO (XO (XI (XI (XI XH)))))))

n243 :: N
n243 =
  Npos (XI (XI (XO (XO (XI (XI (XI XH)))))))

n244 :: N
n244 =
  Npos (XO (XO (XI (XO (XI (XI (XI XH)))))))

n245 :: N
n245 =
  Npos (XI (XO (XI (XO (XI (XI (XI XH)))))))

n246 :: N
n246 =
  Npos (XO (XI (XI (XO (XI (XI (XI XH)))))))

n247 :: N
n247 =
  Npos (XI (XI (XI (XO (XI (XI (XI XH)))))))

n248 :: N
n248 =
  Npos (XO (XO (XO (XI (XI (XI (XI XH)))))))

n249 :: N
n249 =
  Npos (XI (XO (XO (XI (XI (XI (XI XH)))))))

n250 :: N
n250 =
  Npos (XO (XI (XO (XI (XI (XI (XI XH)))))))

n251 :: N
n251 =
  Npos (XI (XI (XO (XI (XI (XI (XI XH)))))))

n252 :: N
n252 =
  Npos (XO (XO (XI (XI (XI (XI (XI XH)))))))

n253 :: N
n253 =
  Npos (XI (XO (XI (XI (XI (XI (XI XH)))))))

n254 :: N
n254 =
  Npos (XO (XI (XI (XI (XI (XI (XI XH)))))))

n255 :: N
n255 =
  Npos (XI (XI (XI (XI (XI (XI (XI XH)))))))

-- generics

-- foo1 = 1

-- deriveGTraversable ''BinderAnnot -- Name 
-- deriveGTraversable ''Branch -- Term) 
-- deriveGTraversable ''Context_decl -- Term) 
-- deriveGTraversable ''Def -- Term) 
-- deriveGTraversable ''List -- (Branch Term))


-- deriveGTraversable ''Allowed_eliminations 
-- deriveGTraversable ''Bool 
-- deriveGTraversable ''Byte 
-- deriveGTraversable ''CaseInfo 
-- deriveGTraversable ''CastKind   
-- deriveGTraversable ''Constant_body 
-- deriveGTraversable ''Constructor_body 
-- deriveGTraversable ''Global_decl 
-- -- deriveGTraversable ''Global_declarations  is a type

-- deriveGTraversable ''Global_env 
-- deriveGTraversable ''Inductive 
-- deriveGTraversable ''Modpath 
-- deriveGTraversable ''MutualInductiveBody 
-- deriveGTraversable ''MyString 
-- deriveGTraversable ''N 
-- deriveGTraversable ''Name 
-- deriveGTraversable ''Nat 
-- deriveGTraversable ''One_inductive_body 

-- deriveGTraversable ''Projection  
-- deriveGTraversable ''Projection_body 
-- deriveGTraversable ''RecursivityKind 
-- deriveGTraversable ''Relevance 
-- deriveGTraversable ''T36 
-- deriveGTraversable ''T37 
-- deriveGTraversable ''T_ 
-- deriveGTraversable ''T_3 
-- deriveGTraversable ''Term 
-- deriveGTraversable ''T_2   
-- deriveGTraversable ''Tree 
-- deriveGTraversable ''Tree0 
-- --instance c Positive => GTraversable c Z
-- deriveGTraversable ''Positive

-- deriveGTraversable ''UniversesDecl 
-- deriveGTraversable ''Z



-- deriving stock instance Generic (Prod Global_env Term)
deriving stock instance Generic (List (Context_decl Term)) 
deriving stock instance Generic (List (Def Term)) 
deriving stock instance Generic (List Aname) 
deriving stock instance Generic (List Constructor_body) 
deriving stock instance Generic (List Elt1) 
deriving stock instance Generic (List Ident) 
deriving stock instance Generic (List One_inductive_body) 
deriving stock instance Generic (List Projection_body) 
deriving stock instance Generic (List T3) 
deriving stock instance Generic (List T36) 
deriving stock instance Generic (List Term) 
deriving stock instance Generic (Option (List T36)) 
deriving stock instance Generic (Option Kername) 
deriving stock instance Generic (Option Term) 
deriving stock instance Generic (Predicate Term)
deriving stock instance Generic (Prod (Prod T3 T24) T3) 
deriving stock instance Generic (Prod Global_env Bool) 
deriving stock instance Generic (Prod Global_env Term)
deriving stock instance Generic (Prod Kername Global_decl) 
deriving stock instance Generic (Prod Modpath Ident) 
deriving stock instance Generic (Prod T10 T32) 
deriving stock instance Generic (Prod T3 Nat) 
deriving stock instance Generic (Prod T3 T24) 


-- next
-- deriving instance GQLResolver (List (Context_decl Term)) 
-- deriving  instance GQLResolver (List (Def Term)) 
-- deriving  instance GQLResolver (List Aname) 
-- deriving  instance GQLResolver (List Constructor_body) 
-- deriving  instance GQLResolver (List Elt1) 
-- deriving  instance GQLResolver (List Ident) 
-- deriving  instance GQLResolver (List One_inductive_body) 
-- deriving  instance GQLResolver (List Projection_body) 
-- deriving  instance GQLResolver (List T3) 
-- deriving  instance GQLResolver (List T36) 
-- deriving  instance GQLResolver (List Term) 
-- deriving  instance GQLResolver (Option (List T36)) 
-- deriving  instance GQLResolver (Option Kername) 
-- deriving  instance GQLResolver (Option Term) 
-- deriving  instance GQLResolver (Predicate Term)
-- deriving  instance GQLResolver (Prod (Prod T3 T24) T3) 
-- deriving  instance GQLResolver (Prod Global_env Bool) 
-- deriving  instance GQLResolver (Prod Global_env Term)
-- deriving  instance GQLResolver (Prod Kername Global_decl) 
-- deriving  instance GQLResolver (Prod Modpath Ident) 
-- deriving  instance GQLResolver (Prod T10 T32) 
-- deriving  instance GQLResolver (Prod T3 Nat) 
-- deriving  instance GQLResolver  (Prod T3 T24) 

-- JSON ENCODINGJUNK 
-- instance ToJSON (Prod Global_env Term) where
--   --toEncoding _ =  toEncoding (Data.Aeson.String "bar")
--   toEncoding = genericToEncoding defaultOptions

-- --instance ToJSON (Prod (Global_declarations T37 Global_env) Bool)) where
-- --  toEncoding = genericToEncoding defaultOptions

-- instance ToJSON Bool where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON (Prod Global_env Bool) where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON (Option Kername) where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON T37 where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (Prod T10 T32) where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Tree where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON  T_3 where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (Prod T3 T24) where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (Prod (Prod T3 T24) T3) where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Tree0 where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Z where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Global_env where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON T_ where
--     toEncoding = genericToEncoding defaultOptions


-- instance ToJSON MyString where
--     toEncoding x = toEncoding (concatChars x)

-- instance ToJSON Byte where
--       toEncoding x = do
--           let v = toString x
--           toEncoding (object [ "name" .= v ])
    
-- instance ToJSON Nat where
--     toEncoding = genericToEncoding defaultOptions
-- instance ToJSON N where
--     toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Positive where
--     toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (List Ident) where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON (Prod Modpath Ident) where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Modpath where
--   toEncoding = genericToEncoding defaultOptions









-- instance ToJSON (List Term) where
--     toEncoding = genericToEncoding defaultOptions
-- instance ToJSON (List T3) where
--     toEncoding = genericToEncoding defaultOptions
    
-- instance ToJSON (List Aname) where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (Branch Term) where
--   toEncoding = genericToEncoding defaultOptions


-- instance ToJSON (List (Branch Term)) where
--   toEncoding = genericToEncoding defaultOptions


-- instance ToJSON (Def Term) where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (List (Def Term)) where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (Predicate Term) where 
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON Inductive where
--   toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Projection where 
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON CastKind  where 
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (Prod T3 Nat) where
--     toEncoding = genericToEncoding defaultOptions
-- instance ToJSON (List Elt1) where
--   toEncoding = genericToEncoding defaultOptions
-- instance  ToJSON T_2 where 
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON Term where
--   toEncoding = genericToEncoding defaultOptions
  
-- instance ToJSON (Option Term) where
--   toEncoding = genericToEncoding defaultOptions
  
-- instance ToJSON (Context_decl Term) where
--   toEncoding = genericToEncoding defaultOptions
  
-- instance ToJSON (List (Context_decl Term)) where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON Relevance where
--    toEncoding = genericToEncoding defaultOptions
-- instance ToJSON Name where
--    toEncoding = genericToEncoding defaultOptions
-- instance ToJSON (BinderAnnot Name) where
--    toEncoding = genericToEncoding defaultOptions


-- instance ToJSON CaseInfo where
--     toEncoding = genericToEncoding defaultOptions
-- instance ToJSON  Constructor_body where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON Projection_body where
--   toEncoding = genericToEncoding defaultOptions

-- instance ToJSON (List Projection_body) where
--   toEncoding = genericToEncoding defaultOptions


-- instance ToJSON Allowed_eliminations where
--   toEncoding = genericToEncoding defaultOptions

-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- instance ToJSON (List Constructor_body) where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- instance ToJSON UniversesDecl where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- instance ToJSON T36 where
-- --   toEncoding = genericToEncoding defaultOptions
  
-- -- instance ToJSON (List T36) where
-- --   toEncoding = genericToEncoding defaultOptions
  
-- -- instance ToJSON (Option (List T36)) where
-- --   toEncoding = genericToEncoding defaultOptions

  
-- -- instance ToJSON One_inductive_body where
-- --   toEncoding = genericToEncoding defaultOptions
    
-- -- instance ToJSON (List One_inductive_body) where
-- --     toEncoding = genericToEncoding defaultOptions
-- -- instance ToJSON RecursivityKind where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- instance ToJSON MutualInductiveBody where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- instance ToJSON Constant_body where
-- --   toEncoding = genericToEncoding defaultOptions
  
-- -- instance ToJSON  Global_decl where
-- --   toEncoding = genericToEncoding defaultOptions
-- -- instance ToJSON (Prod Kername Global_decl) where
-- --       toEncoding = genericToEncoding defaultOptions



-- -- instance ToJSON Global_declarations where
-- --     toEncoding = genericToEncoding defaultOptions


-- --instance ToJSON GProgram where
-- --    toEncoding x =
-- --      case x of {
-- --        Mk_global_env a lp c -> toEncoding (Data.Aeson.String "foo") ;
--           --Cons _ _ -> "Foo";
--           -- _ -> toEncoding x
-- --          _ -> toEncoding (Data.Aeson.String "bar" ) ;
-- --          }
--       --genericToEncoding defaultOptions

-- --type ListProgram = List Program
-- --             deriving ( Generic, Show, GQLType, Data, Typeable)
-- --type ListMyString = List MyString
-- --             deriving ( Generic, Show, GQLType, Data, Typeable)
-- --instance ToJSON ListProgram where
-- --  toEncoding = genericToEncoding defaultOptions
-- --instance ToJSON Program where
-- --  toEncoding = genericToEncoding defaultOptions

-- --instance ToJSON (Global_declarations T37 Global_env)  where
-- --  toEncoding = genericToEncoding defaultOptions
  
-- --instance ToJSON (Prod (Global_declarations T37 Global_env) TestMeta.Bool) where
-- --  toEncoding = genericToEncoding defaultOptions


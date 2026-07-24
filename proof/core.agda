{-# OPTIONS --rewriting #-}

open import Data.Nat
open import Data.Fin
open import Data.Vec hiding ([_])
open import Data.Empty
open import Data.Product hiding (map)
open import Data.Sum hiding (map)
open import Relation.Binary.PropositionalEquality hiding ([_])
open import Relation.Nullary.Decidable hiding (map)
{-# BUILTIN REWRITE _≡_ #-}

record _≅_ (A B : Set) : Set where
    field 
        to : A -> B 
        from : B -> A
        fromto : ∀{a} -> from (to a) ≡ a
        tofrom : ∀{b} -> to (from b) ≡ b

postulate 
    funext : {A B : Set} -> {f g : A -> B} -> ((x : A) -> f x ≡ g x) -> f ≡ g
    Constructor : Set
    Var : Set 
    Cleft : Var ≅ (Var ⊎ Var)
    _≟v_ : (x y : Var) -> Dec (x ≡ y)
    Fresh : ℕ -> Var 
    Fresh-inj : {x y : ℕ} -> (Fresh x ≡ Fresh y) -> x ≡ y

L : Var -> Var 
L x = _≅_.from Cleft (inj₁ x)

R : Var -> Var 
R x = _≅_.from Cleft (inj₂ x)

cleave : Var -> Var ⊎ Var
cleave = _≅_.to Cleft

fromto : {a : Var} → Cleft ._≅_.from (Cleft ._≅_.to a) ≡ a
fromto = _≅_.fromto Cleft 

tofrom : {b : Var ⊎ Var} → Cleft ._≅_.to (Cleft ._≅_.from b) ≡ b
tofrom = _≅_.tofrom Cleft 

{-# REWRITE fromto #-}
{-# REWRITE tofrom #-}

data Pattern : Set where 
    X : Var -> Pattern
    K : Constructor -> (n : ℕ) -> Vec Pattern n -> Pattern 

Sub : Set 
Sub = Var -> Pattern

{-# TERMINATING #-}
_[_] : Sub -> Pattern -> Pattern 
s [ X x ] = s x
s [ K k n ps ] = K k n (map (λ p -> (s [ p ])) ps)

infixr 30 _[_]

sid : Sub 
sid x = X x 

mutual 
    map-sid-eq : ∀{n} 
        -> (ps : Vec Pattern n)
        -> map (_[_] sid) ps ≡ ps
    map-sid-eq [] = refl
    map-sid-eq (p ∷ ps) 
        rewrite sid-eq p 
        rewrite map-sid-eq ps = refl
    
    sid-eq : (p : Pattern)
        -> sid [ p ] ≡ p 
    sid-eq (X x) = refl
    sid-eq (K k n ps)
        rewrite map-sid-eq ps = refl

{-# REWRITE sid-eq #-}

functional : Pattern -> Pattern -> Set
functional p1 p2 = (s1 s2 : Sub)
    -> s1 [ p1 ] ≡ s2 [ p1 ]
    -> s1 [ p2 ] ≡ s2 [ p2 ]

data Rule : Set where 
    _↦_[_] : (p1 p2 : Pattern) -> functional p1 p2 -> Rule

data _≡r_ : Rule -> Rule -> Set where
    REquiv : ∀{p1 p2 p3 p4 f1 f2}
        -> (s1 s2 : Sub)
        -> s1 [ p1 ] ≡ p3
        -> s1 [ p2 ] ≡ p4
        -> s2 [ p3 ] ≡ p1
        -> s2 [ p4 ] ≡ p2
        -> (p1 ↦ p2 [ f1 ]) ≡r (p3 ↦ p4 [ f2 ])

_∘_ : Sub -> Sub -> Sub 
(s1 ∘ s2) x = s1 [ s2 x ]

data _⊑_ (s1 s2 : Sub) : Set where 
    Prec : (s : Sub) -> s1 ≡ s ∘ s2 -> s1 ⊑ s2

data _,_unifies_,_ (s1 s2 : Sub) (p1 p2 : Pattern) : Set where
    Unify : s1 [ p1 ] ≡ s2 [ p2 ]
        -> s1 , s2 unifies p1 , p2

data _,_mgu_,_ (s1 s2 : Sub) (p1 p2 : Pattern) : Set where
    MGU : s1 , s2 unifies p1 , p2
        -> ((s1' s2' : Sub) 
            -> s1' , s2' unifies p1 , p2
            -> (s1' ⊑ s1 × s2' ⊑ s2))
        -> s1 , s2 mgu p1 , p2

mutual 
    map-fusion : ∀{s1 s2 n} 
        -> {ps : Vec Pattern n}
        -> map (_[_] s1) (map (_[_] s2) ps) ≡ map (_[_] (s1 ∘ s2)) ps
    map-fusion {ps = []} = refl
    map-fusion {s1} {s2} {ps = p ∷ ps} 
        rewrite ∘-eq s1 s2 p 
        rewrite map-fusion {s1} {s2} {ps = ps} = refl

    ∘-eq : (s1 s2 : Sub)
        -> (p : Pattern)
        -> (s1 ∘ s2) [ p ] ≡ s1 [ s2 [ p ] ]
    ∘-eq _ _ (X x) = refl
    ∘-eq s1 s2 (K k n ps)
        rewrite map-fusion {s1} {s2} {ps = ps} = refl

{-# REWRITE ∘-eq #-}

∘r-functional : (p1 p2 p3 p4 : Pattern)
        -> (f1 : functional p1 p2) 
        -> (f2 : functional p3 p4) 
        -> (s1 s2 : Sub)
        -> (mgu : s1 , s2 mgu p2 , p3)
        -> functional (s1 [ p1 ]) (s2 [ p4 ])
∘r-functional p1 p2 p3 p4 f1 f2 s1 s2 (MGU (Unify u) _) s3 s4 eq1 with f1 (s3 ∘ s1) (s4 ∘ s1) eq1 
... | eq2 rewrite u = f2 (s3 ∘ s2) (s4 ∘ s2) eq2

data _∘r_≡_ : Rule -> Rule -> Rule -> Set where
    Comp : ∀{p1 p2 p3 p4}
        -> (f1 : functional p1 p2) 
        -> (f2 : functional p3 p4) 
        -> (s1 s2 : Sub)
        -> (mgu : s1 , s2 mgu p2 , p3)
        -> (p1 ↦ p2 [ f1 ]) ∘r (p3 ↦ p4 [ f2 ]) ≡ (s1 [ p1 ] ↦ s2 [ p4 ] [ ∘r-functional p1 p2 p3 p4 f1 f2 s1 s2 mgu ]) 

data _↦[_]_ : Pattern -> Rule -> Pattern -> Set where 
    Step : (t1 t2 p1 p2 : Pattern)
        -> (f : functional p1 p2)
        -> (s : Sub)
        -> t1 ≡ s [ p1 ]
        -> t2 ≡ s [ p2 ]
        -> t1 ↦[ p1 ↦ p2 [ f ] ] t2

Ruleset : Set₁ 
Ruleset = Rule -> Set 

data _↦*[_]_ : Pattern -> Ruleset -> Pattern -> Set₁ where 
    Refl : (p : Pattern) 
        -> (R : Ruleset) 
        -> p ↦*[ R ] p
    Cons : ∀{p1 p2 p3 R r}
        -> R r
        -> p1 ↦[ r ] p2
        -> p2 ↦*[ R ] p3
        -> p1 ↦*[ R ] p3

data _↦+[_]_ : Pattern -> Ruleset -> Pattern -> Set₁ where 
    Step : ∀{p1 p2 R r}
        -> R r
        -> p1 ↦[ r ] p2
        -> p1 ↦+[ R ] p2
    Cons : ∀{p1 p2 p3 R r}
        -> R r
        -> p1 ↦[ r ] p2
        -> p2 ↦+[ R ] p3
        -> p1 ↦+[ R ] p3

_↦̸[_] : Pattern -> Ruleset -> Set
p ↦̸[ R ] = (p' : Pattern) 
    -> (r : Rule) 
    -> R r 
    -> p ↦[ r ] p'
    -> ⊥

data _=>[_]_ : Pattern -> Ruleset -> Pattern -> Set₁ where 
    Eval : ∀{p1 p2 R}
        -> p1 ↦*[ R ] p2 
        -> p2 ↦̸[ R ]
        -> p1 =>[ R ] p2

data _≅R_ (R1 R2 : Ruleset) : Set₁ where 
    Equiv : (∀{p1 p2} 
            -> p1 =>[ R1 ] p2
            -> p1 =>[ R2 ] p2)
        -> (∀{p1 p2} 
            -> p1 =>[ R2 ] p2
            -> p1 =>[ R1 ] p2)
        -> R1 ≅R R2

_∪[_] : Ruleset -> Rule -> Ruleset 
(R ∪[ r ]) r' = R r' ⊎ r' ≡ r

infixr 30 _∪[_]
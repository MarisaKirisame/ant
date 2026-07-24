{-# OPTIONS --rewriting #-}


open import Data.Nat
open import Data.Nat.Properties
open import Data.Vec hiding ([_])
open import Data.Empty
open import Data.Unit
open import Data.Fin hiding (_+_; _<_; _≤_; _>_)
open import Data.Product hiding (map)
open import Data.Sum hiding (map)
open import Relation.Binary.PropositionalEquality hiding ([_])
open import Relation.Nullary.Decidable hiding (map)
open import Data.List using (List; []; _∷_)

open import core

{-# TERMINATING #-}
size : Pattern -> ℕ 
size (X x) = 0
size (K k n ps) = 1 + sum (map size ps)

{-# TERMINATING #-}
size-diff : (s : Sub) -> (p : Pattern) -> ℕ
size-diff s (X x) = size (s x)
size-diff s (K k n ps) = sum (map (size-diff s) ps)

metric : (s1 s2 : Sub) -> (p1 p2 : Pattern) -> ℕ
metric s1 s2 p1 p2 = size-diff s1 p1 + size-diff s2 p2

mutual 
    equiv-constructors : ∀{n} 
        -> (ps1 ps2 : Vec Pattern n) 
        -> Set
    equiv-constructors [] [] = ⊤
    equiv-constructors (p1 ∷ ps1) (p2 ∷ ps2) = equiv-constructor p1 p2 × equiv-constructors ps1 ps2

    data equiv-constructor : (p1 p2 : Pattern) -> Set where 
        ECX : ∀{x1 x2} 
            -> equiv-constructor (X x1) (X x2)
        ECK : ∀{k n ps1 ps2} 
            -> equiv-constructors ps1 ps2
            -> equiv-constructor (K k n ps1) (K k n ps2)

mutual 
    equiv-constructors-sym : ∀{n} 
        -> {ps1 ps2 : Vec Pattern n}
        -> equiv-constructors ps1 ps2
        -> equiv-constructors ps2 ps1
    equiv-constructors-sym {zero} {[]} {[]} ecs = tt
    equiv-constructors-sym {suc _} {p1 ∷ ps1} {p2 ∷ ps2} (ec , ecs) = equiv-constructor-sym ec , equiv-constructors-sym ecs

    equiv-constructor-sym : ∀{p1 p2}
        -> equiv-constructor p1 p2 
        -> equiv-constructor p2 p1
    equiv-constructor-sym ECX = ECX
    equiv-constructor-sym (ECK ecs) = ECK (equiv-constructors-sym ecs)

mutual 
    equiv-constructors-trans : ∀{n} 
        -> {ps1 ps2 ps3 : Vec Pattern n}
        -> equiv-constructors ps1 ps2
        -> equiv-constructors ps2 ps3
        -> equiv-constructors ps1 ps3
    equiv-constructors-trans {zero} {[]} {[]} {[]} _ _ = tt
    equiv-constructors-trans {suc _} {p1 ∷ ps1} {p2 ∷ ps2} {p3 ∷ ps3} (ec1 , ecs1) (ec2 , ecs2) = (equiv-constructor-trans ec1 ec2) , equiv-constructors-trans ecs1 ecs2

    equiv-constructor-trans : ∀{p1 p2 p3}
        -> equiv-constructor p1 p2 
        -> equiv-constructor p2 p3
        -> equiv-constructor p1 p3
    equiv-constructor-trans ECX ECX = ECX
    equiv-constructor-trans (ECK ecs1) (ECK ecs2) = ECK (equiv-constructors-trans ecs1 ecs2)

mutual 
    size-diff-zero-map : ∀{n}
        -> (s : Sub)
        -> (ps : Vec Pattern n)
        -> sum (map (size-diff s) ps) ≡ zero
        -> equiv-constructors ps (map (_[_] s) ps)
    size-diff-zero-map s [] eq = tt
    size-diff-zero-map s (p ∷ ps) eq1 with size-diff s p in eq2
    ... | zero = size-diff-zero s p eq2 , size-diff-zero-map s ps eq1

    size-diff-zero : (s : Sub)
        -> (p : Pattern)
        -> size-diff s p ≡ zero
        -> equiv-constructor p (s [ p ]) 
    size-diff-zero s (X x) eq with s x 
    size-diff-zero s (X x) eq | X _ = ECX
    size-diff-zero s (K k n ps) eq = ECK (size-diff-zero-map s ps eq)

-- begin machine generated code 

VarSub : Set
VarSub = Var -> Var

toSub : VarSub -> Sub
toSub f x = X (f x)

_∘v_ : VarSub -> VarSub -> VarSub
(f ∘v g) x = f (g x)

rename2 : Var -> Var -> Var -> VarSub
rename2 a b c v with a ≟v v
... | yes _ = c
... | no _ with b ≟v v
... | yes _ = c
... | no _ = v

rename2-a : (a b c : Var) -> rename2 a b c a ≡ c
rename2-a a b c with a ≟v a
... | yes _ = refl
... | no neq = ⊥-elim (neq refl)

rename2-b : (a b c : Var) -> rename2 a b c b ≡ c
rename2-b a b c with a ≟v b
... | yes _ = refl
... | no _ with b ≟v b
... | yes _ = refl
... | no neq = ⊥-elim (neq refl)

X-inj : ∀{a b} -> _≡_ {_} {Pattern} (X a) (X b) -> a ≡ b
X-inj refl = refl

-- L-injectivity from Cleft isomorphism
L-inj : {x y : Var} -> L x ≡ L y -> x ≡ y
L-inj eq with cong (_≅_.to Cleft) eq
... | ceq = inj₁-inj ceq
    where
    inj₁-inj : ∀{A B : Set}{a b : A} -> _≡_ {_} {A ⊎ B} (inj₁ a) (inj₁ b) -> a ≡ b
    inj₁-inj refl = refl

-- R-injectivity from Cleft isomorphism
R-inj : {x y : Var} -> R x ≡ R y -> x ≡ y
R-inj eq with cong (_≅_.to Cleft) eq
... | ceq = inj₂-inj ceq
    where
    inj₂-inj : ∀{A B : Set}{a b : B} -> _≡_ {_} {A ⊎ B} (inj₂ a) (inj₂ b) -> a ≡ b
    inj₂-inj refl = refl

-- L and R are disjoint (from inj₁ ≢ inj₂)
L-R-disjoint : {x y : Var} -> L x ≢ R y
L-R-disjoint eq with cong (_≅_.to Cleft) eq
... | ceq = inj₁≢inj₂ ceq
    where
    inj₁≢inj₂ : ∀{A B : Set}{a : A}{b : B} -> _≡_ {_} {A ⊎ B} (inj₁ a) (inj₂ b) -> ⊥
    inj₁≢inj₂ ()

-- Informative case analysis on rename2
rename2-cases : (a b c v : Var)
    -> ((a ≡ v) × (rename2 a b c v ≡ c))
     ⊎ ((a ≢ v) × (b ≡ v) × (rename2 a b c v ≡ c))
     ⊎ ((a ≢ v) × (b ≢ v) × (rename2 a b c v ≡ v))
rename2-cases a b c v with a ≟v v
... | yes eq = inj₁ (eq , refl)
... | no neqa with b ≟v v
... | yes eq = inj₂ (inj₁ (neqa , eq , refl))
... | no neqb = inj₂ (inj₂ (neqa , neqb , refl))

K-inj-ps : ∀{k1 k2 n} {ps1 ps2 : Vec Pattern n}
    -> _≡_ {_} {Pattern} (K k1 n ps1) (K k2 n ps2) -> ps1 ≡ ps2
K-inj-ps refl = refl

cons-inj-hd : ∀{n}{a b : Pattern}{as bs : Vec Pattern n} -> a ∷ as ≡ b ∷ bs -> a ≡ b
cons-inj-hd refl = refl

cons-inj-tl : ∀{n}{a b : Pattern}{as bs : Vec Pattern n} -> a ∷ as ≡ b ∷ bs -> as ≡ bs
cons-inj-tl refl = refl

mutual
    lift-ec-vec : ∀{n} {ps1 ps2 : Vec Pattern n}
        -> (h : VarSub) (f g : VarSub)
        -> (ecs : equiv-constructors ps1 ps2)
        -> map (_[_] (toSub f)) ps1 ≡ map (_[_] (toSub g)) ps2
        -> map (_[_] (toSub (h ∘v f))) ps1 ≡ map (_[_] (toSub (h ∘v g))) ps2
    lift-ec-vec {ps1 = []} {[]} h f g _ _ = refl
    lift-ec-vec {ps1 = _ ∷ _} {_ ∷ _} h f g (ec , ecs) eq =
        cong₂ _∷_ (lift-ec h f g ec (cons-inj-hd eq)) (lift-ec-vec h f g ecs (cons-inj-tl eq))

    lift-ec : ∀{p1 p2}
        -> (h : VarSub) (f g : VarSub)
        -> (ec : equiv-constructor p1 p2)
        -> toSub f [ p1 ] ≡ toSub g [ p2 ]
        -> toSub (h ∘v f) [ p1 ] ≡ toSub (h ∘v g) [ p2 ]
    lift-ec h f g ECX eq = cong (λ v → X (h v)) (X-inj eq)
    lift-ec h f g (ECK {k} {n} ecs) eq =
        cong (K k n) (lift-ec-vec h f g ecs (K-inj-ps eq))

-- Lookup and sp
LookupEntry : Set
LookupEntry = Var × Var

lookup-search : List LookupEntry -> Var -> Var
lookup-search [] w = w
lookup-search ((fv , xv) ∷ rest) w with fv ≟v w
... | yes _ = xv
... | no _ = lookup-search rest w

sp-of : List LookupEntry -> Sub -> Sub -> Sub
sp-of entries s1' s2' v with cleave v
... | inj₂ w = s1' (lookup-search entries w)
... | inj₁ w with cleave w
...   | inj₁ u = s1' u
...   | inj₂ u = s2' u

lookup-hit : (fv xv : Var) (rest : List LookupEntry)
    -> lookup-search ((fv , xv) ∷ rest) fv ≡ xv
lookup-hit fv xv rest with fv ≟v fv
... | yes _ = refl
... | no neq = ⊥-elim (neq refl)

lookup-miss : (fv xv w : Var) (rest : List LookupEntry)
    -> fv ≢ w -> lookup-search ((fv , xv) ∷ rest) w ≡ lookup-search rest w
lookup-miss fv xv w rest neq with fv ≟v w
... | yes eq = ⊥-elim (neq eq)
... | no _ = refl

-- Freshness: R(Fresh j) not in range of f or g for j ≥ k
FreshFrom : VarSub -> VarSub -> ℕ -> Set
FreshFrom f g k = (j : ℕ) -> k ≤ j -> (v : Var) -> f v ≢ R (Fresh j) × g v ≢ R (Fresh j)

rename2-fresh-lem : (a b : Var) (f g : VarSub) (k : ℕ)
    -> FreshFrom f g k
    -> FreshFrom (rename2 a b (R (Fresh k)) ∘v f) (rename2 a b (R (Fresh k)) ∘v g) (suc k)
rename2-fresh-lem a b f g k fr j le v = left-part , right-part
    where
    k≠j : k ≢ j
    k≠j eq = 1+n≰n (subst (suc k ≤_) (sym eq) le)
    RFk≠RFj : R (Fresh k) ≢ R (Fresh j)
    RFk≠RFj eq = k≠j (Fresh-inj (R-inj eq))
    k≤j : k ≤ j
    k≤j = ≤-trans (n≤1+n k) le
    left-part : rename2 a b (R (Fresh k)) (f v) ≢ R (Fresh j)
    left-part eq with rename2-cases a b (R (Fresh k)) (f v)
    ... | inj₁ (_ , is-c) = RFk≠RFj (trans (sym is-c) eq)
    ... | inj₂ (inj₁ (_ , _ , is-c)) = RFk≠RFj (trans (sym is-c) eq)
    ... | inj₂ (inj₂ (_ , _ , is-v)) = proj₁ (fr j k≤j v) (trans (sym is-v) eq)
    right-part : rename2 a b (R (Fresh k)) (g v) ≢ R (Fresh j)
    right-part eq with rename2-cases a b (R (Fresh k)) (g v)
    ... | inj₁ (_ , is-c) = RFk≠RFj (trans (sym is-c) eq)
    ... | inj₂ (inj₁ (_ , _ , is-c)) = RFk≠RFj (trans (sym is-c) eq)
    ... | inj₂ (inj₂ (_ , _ , is-v)) = proj₂ (fr j k≤j v) (trans (sym is-v) eq)

initial-fresh : FreshFrom (L ∘v L) (L ∘v R) 0
initial-fresh j _ v = (λ eq → L-R-disjoint eq) , (λ eq → L-R-disjoint eq)

-- With the new sp-of, L-namespace values don't use entries at all,
-- so extending entries is a no-op for them. Only R-namespace values use entries.

sp-of-extend-miss : (fk : ℕ) (x1 : Var) (ent : List LookupEntry) (s1' s2' : Sub) (v : Var)
    -> v ≢ R (Fresh fk)
    -> sp-of ((Fresh fk , x1) ∷ ent) s1' s2' v ≡ sp-of ent s1' s2' v
sp-of-extend-miss fk x1 ent s1' s2' v neq with cleave v in cv
... | inj₁ w = refl
... | inj₂ w = cong s1' (lookup-miss (Fresh fk) x1 w ent w≠Fk)
    where
    v≡Rw : v ≡ R w
    v≡Rw = trans (sym fromto) (cong (_≅_.from Cleft) cv)
    w≠Fk : Fresh fk ≢ w
    w≠Fk eq = neq (trans v≡Rw (cong R (sym eq)))

-- sp-of on a hit: R(Fresh fk) maps to s1' x1
sp-of-extend-hit : (fk : ℕ) (x1 : Var) (ent : List LookupEntry) (s1' s2' : Sub)
    -> sp-of ((Fresh fk , x1) ∷ ent) s1' s2' (R (Fresh fk)) ≡ s1' x1
sp-of-extend-hit fk x1 ent s1' s2' = cong s1' (lookup-hit (Fresh fk) x1 ent)

-- Result records
record Solved {p1 p2 : Pattern} (ec : equiv-constructor p1 p2)
    (f g : VarSub) (ent : List LookupEntry) (k : ℕ) : Set where
    constructor MkSolved
    field
        f' g' : VarSub
        ent' : List LookupEntry
        k' : ℕ
        h : VarSub
        f'-def : ∀ v -> f' v ≡ h (f v)
        g'-def : ∀ v -> g' v ≡ h (g v)
        unifies : toSub f' [ p1 ] ≡ toSub g' [ p2 ]
        fresh-inv : FreshFrom f' g' k'
        mgu-inv : (s1' s2' : Sub) -> (_,_unifies_,_ s1' s2' p1 p2)
            -> ((v : Var) -> sp-of ent s1' s2' (f v) ≡ s1' v)
            -> ((v : Var) -> sp-of ent s1' s2' (g v) ≡ s2' v)
            -> ((v : Var) -> sp-of ent' s1' s2' (f' v) ≡ s1' v)
             × ((v : Var) -> sp-of ent' s1' s2' (g' v) ≡ s2' v)

record SolvedVec {n : ℕ} {ps1 ps2 : Vec Pattern n}
    (ecs : equiv-constructors ps1 ps2)
    (f g : VarSub) (ent : List LookupEntry) (k : ℕ) : Set where
    constructor MkSolvedVec
    field
        f' g' : VarSub
        ent' : List LookupEntry
        k' : ℕ
        h : VarSub
        f'-def : ∀ v -> f' v ≡ h (f v)
        g'-def : ∀ v -> g' v ≡ h (g v)
        unifies : map (_[_] (toSub f')) ps1 ≡ map (_[_] (toSub g')) ps2
        fresh-inv : FreshFrom f' g' k'
        mgu-inv : (s1' s2' : Sub)
            -> map (_[_] s1') ps1 ≡ map (_[_] s2') ps2
            -> ((v : Var) -> sp-of ent s1' s2' (f v) ≡ s1' v)
            -> ((v : Var) -> sp-of ent s1' s2' (g v) ≡ s2' v)
            -> ((v : Var) -> sp-of ent' s1' s2' (f' v) ≡ s1' v)
             × ((v : Var) -> sp-of ent' s1' s2' (g' v) ≡ s2' v)

mutual
    solve-vec : ∀{n} {ps1 ps2 : Vec Pattern n}
        -> (ecs : equiv-constructors ps1 ps2)
        -> (f g : VarSub) -> (ent : List LookupEntry) -> (k : ℕ)
        -> FreshFrom f g k
        -> SolvedVec ecs f g ent k
    solve-vec {ps1 = []} {[]} _ f g ent k fr =
        MkSolvedVec f g ent k (λ v → v) (λ _ → refl) (λ _ → refl) refl fr
            (λ s1' s2' _ fi gi → fi , gi)
    solve-vec {ps1 = p1 ∷ _} {p2 ∷ _} (ec , ecs) f g ent k fr with solve ec f g ent k fr
    ... | MkSolved f1 g1 ent1 k1 h1 f1d g1d u1 fr1 m1 with solve-vec ecs f1 g1 ent1 k1 fr1
    ... | MkSolvedVec f2 g2 ent2 k2 h2 f2d g2d u2 fr2 m2 = MkSolvedVec f2 g2 ent2 k2 (h2 ∘v h1)
        (λ v → trans (f2d v) (cong h2 (f1d v)))
        (λ v → trans (g2d v) (cong h2 (g1d v)))
        (cong₂ _∷_ head-eq u2)
        fr2
        mgu-combined
        where
        f2≡ : toSub f2 ≡ toSub (h2 ∘v f1)
        f2≡ = funext (λ v → cong X (f2d v))
        g2≡ : toSub g2 ≡ toSub (h2 ∘v g1)
        g2≡ = funext (λ v → cong X (g2d v))
        head-eq : toSub f2 [ p1 ] ≡ toSub g2 [ p2 ]
        head-eq rewrite f2≡ | g2≡ = lift-ec h2 f1 g1 ec u1

        mgu-combined : (s1' s2' : Sub)
            -> map (_[_] s1') (p1 ∷ _) ≡ map (_[_] s2') (p2 ∷ _)
            -> ((v : Var) -> sp-of ent s1' s2' (f v) ≡ s1' v)
            -> ((v : Var) -> sp-of ent s1' s2' (g v) ≡ s2' v)
            -> ((v : Var) -> sp-of ent2 s1' s2' (f2 v) ≡ s1' v)
             × ((v : Var) -> sp-of ent2 s1' s2' (g2 v) ≡ s2' v)
        mgu-combined s1' s2' u' fi gi with m1 s1' s2' (Unify (cons-inj-hd u')) fi gi
        ... | fi1 , gi1 = m2 s1' s2' (cons-inj-tl u') fi1 gi1

    solve : ∀{p1 p2}
        -> (ec : equiv-constructor p1 p2)
        -> (f g : VarSub) -> (ent : List LookupEntry) -> (k : ℕ)
        -> FreshFrom f g k
        -> Solved ec f g ent k
    solve (ECX {x1} {x2}) f g ent k fr with f x1 ≟v g x2
    ... | yes veq = MkSolved f g ent k (λ v → v)
        (λ _ → refl) (λ _ → refl) (cong X veq) fr
        (λ s1' s2' _ fi gi → fi , gi)
    ... | no neq = MkSolved
        (rename2 (f x1) (g x2) (R (Fresh k)) ∘v f)
        (rename2 (f x1) (g x2) (R (Fresh k)) ∘v g)
        ((Fresh k , x1) ∷ ent)
        (suc k)
        (rename2 (f x1) (g x2) (R (Fresh k)))
        (λ _ → refl) (λ _ → refl)
        (cong X (trans (rename2-a (f x1) (g x2) (R (Fresh k)))
                       (sym (rename2-b (f x1) (g x2) (R (Fresh k))))))
        (rename2-fresh-lem (f x1) (g x2) f g k fr)
        mgu-leaf-wrap
        where
        ren : VarSub
        ren = rename2 (f x1) (g x2) (R (Fresh k))

        sp-of-ren : (a b : Var) (fk : ℕ) (x₁ : Var) (ent₁ : List LookupEntry) (s1' s2' : Sub) (w : Var)
            -> sp-of ent₁ s1' s2' a ≡ s1' x₁
            -> sp-of ent₁ s1' s2' b ≡ s1' x₁
            -> w ≢ R (Fresh fk)
            -> sp-of ((Fresh fk , x₁) ∷ ent₁) s1' s2' (rename2 a b (R (Fresh fk)) w) ≡ sp-of ent₁ s1' s2' w
        sp-of-ren a b fk x₁ ent₁ s1' s2' w spa spb wfr with rename2-cases a b (R (Fresh fk)) w
        ... | inj₁ (a≡w , ren≡c) rewrite ren≡c =
            trans (sp-of-extend-hit fk x₁ ent₁ s1' s2')
                  (trans (sym spa) (cong (sp-of ent₁ s1' s2') a≡w))
        ... | inj₂ (inj₁ (_ , b≡w , ren≡c)) rewrite ren≡c =
            trans (sp-of-extend-hit fk x₁ ent₁ s1' s2')
                  (trans (sym spb) (cong (sp-of ent₁ s1' s2') b≡w))
        ... | inj₂ (inj₂ (_ , _ , ren≡v)) rewrite ren≡v =
            sp-of-extend-miss fk x₁ ent₁ s1' s2' w wfr

        mgu-leaf-wrap : (s1' s2' : Sub) -> (_,_unifies_,_ s1' s2' (X x1) (X x2))
            -> ((v : Var) -> sp-of ent s1' s2' (f v) ≡ s1' v)
            -> ((v : Var) -> sp-of ent s1' s2' (g v) ≡ s2' v)
            -> ((v : Var) -> sp-of ((Fresh k , x1) ∷ ent) s1' s2' (ren (f v)) ≡ s1' v)
             × ((v : Var) -> sp-of ((Fresh k , x1) ∷ ent) s1' s2' (ren (g v)) ≡ s2' v)
        mgu-leaf-wrap s1' s2' (Unify u') fi gi = left-inv , right-inv
            where
            left-inv : (v : Var) -> sp-of ((Fresh k , x1) ∷ ent) s1' s2' (ren (f v)) ≡ s1' v
            left-inv v = trans (sp-of-ren (f x1) (g x2) k x1 ent s1' s2' (f v)
                (fi x1) (trans (gi x2) (sym u')) (proj₁ (fr k ≤-refl v))) (fi v)
            right-inv : (v : Var) -> sp-of ((Fresh k , x1) ∷ ent) s1' s2' (ren (g v)) ≡ s2' v
            right-inv v = trans (sp-of-ren (f x1) (g x2) k x1 ent s1' s2' (g v)
                (fi x1) (trans (gi x2) (sym u')) (proj₂ (fr k ≤-refl v))) (gi v)

    solve (ECK {kk} {n} ecs) f g ent k fr with solve-vec ecs f g ent k fr
    ... | MkSolvedVec f' g' ent' k' h fd gd u fr' m = MkSolved f' g' ent' k' h fd gd (cong (K kk n) u) fr' mgu-k
        where
        mgu-k : (s1' s2' : Sub) -> (_,_unifies_,_ s1' s2' (K kk n _) (K kk n _))
            -> ((v : Var) -> sp-of ent s1' s2' (f v) ≡ s1' v)
            -> ((v : Var) -> sp-of ent s1' s2' (g v) ≡ s2' v)
            -> ((v : Var) -> sp-of ent' s1' s2' (f' v) ≡ s1' v)
             × ((v : Var) -> sp-of ent' s1' s2' (g' v) ≡ s2' v)
        mgu-k s1' s2' (Unify u') fi gi = m s1' s2' (K-inj-ps u') fi gi

generalization-equiv-constructor : ∀{p1 p2}
    -> equiv-constructor p1 p2
    -> ∃[ s1 ] ∃[ s2 ] s1 , s2 mgu p1 , p2
generalization-equiv-constructor ec with solve ec (L ∘v L) (L ∘v R) [] 0 initial-fresh
... | MkSolved f g ent' k' h fd gd u fr m = toSub f , toSub g , MGU (Unify u) mgu'
    where
    mgu' : (s1' s2' : Sub) -> (_,_unifies_,_ s1' s2' _ _)
        -> s1' ⊑ toSub f × s2' ⊑ toSub g
    mgu' s1' s2' u' with m s1' s2' u' (λ v → refl) (λ v → refl)
    ... | fi , gi = Prec (sp-of ent' s1' s2') (funext (λ v → sym (fi v)))
                  , Prec (sp-of ent' s1' s2') (funext (λ v → sym (gi v)))

-- end machine generated code

data _,_preunifies_,_ (s1 s2 : Sub) (p1 p2 : Pattern) : Set where
    PU : ((s1' s2' : Sub) 
            -> s1' , s2' unifies p1 , p2
            -> (s1' ⊑ s1 × s2' ⊑ s2))
        -> s1 , s2 preunifies p1 , p2

freshesL : (n : ℕ) -> Vec Pattern n 
freshesL zero = []
freshesL (suc n) = X (L (Fresh n)) ∷ freshesL n

freshesL-ex : freshesL 3 ≡ (X (L (Fresh 2))) ∷ (X (L (Fresh 1)) ∷ (X (L (Fresh 0))) ∷ [])
freshesL-ex = refl

childfold : {n : ℕ} -> (s : Sub) -> (ps : Vec Pattern n) -> Sub
childfold s [] x = X x
childfold {suc n} s (p ∷ ps) x with x ≟v (Fresh n)
... | yes refl = s [ p ]
... | no neq = childfold s ps x

data prefix {n : ℕ} (ps : Vec Pattern n) : {n' : ℕ} -> (ps' : Vec Pattern n') -> Set where 
    PrefixSelf : prefix ps ps
    PrefixCons : ∀{p' n'}
        -> {ps' : Vec Pattern n'}
        -> prefix ps (p' ∷ ps')
        -> prefix ps ps'

data indexof : {n : ℕ} -> (ps : Vec Pattern n) -> (n' : ℕ) -> (p : Pattern) -> Set where 
    IndexOfHead : ∀{n p}
        -> {ps : Vec Pattern n}
        -> indexof (p ∷ ps) n p
    IndexOfCons : ∀{n n' p p'}
        -> {ps : Vec Pattern n'}
        -> indexof ps n p
        -> indexof (p' ∷ ps) n p

prefix-to-index : ∀{n1 n1' n p}
    -> {ps : Vec Pattern n1}
    -> {ps' : Vec Pattern n1'}
    -> prefix ps ps'
    -> indexof ps' n p
    -> indexof ps n p
prefix-to-index PrefixSelf i = i
prefix-to-index (PrefixCons p) i = prefix-to-index p (IndexOfCons i)

index-lt-length : ∀{n n' p}
    -> {ps : Vec Pattern n'}
    -> indexof ps n p
    -> n < n'
index-lt-length IndexOfHead = s≤s (≤-reflexive refl)
index-lt-length (IndexOfCons i) = helper (index-lt-length i)
    where 
    helper : ∀{n n'} -> n ≤ n' -> n ≤ suc n'
    helper z≤n = z≤n
    helper (s≤s leq) = s≤s (helper leq)

lt-not-eq : ∀{n n'} -> n < n' -> n ≡ n' -> ⊥ 
lt-not-eq (s≤s l) refl = lt-not-eq l refl

cons-inj : ∀{n x1 x2}
    -> {xs1 xs2 : Vec Pattern n}
    -> x1 ∷ xs1 ≡ x2 ∷ xs2 
    -> x1 ≡ x2
cons-inj refl = refl

-- cons-inj-tl : ∀{n x1 x2}
--     -> {xs1 xs2 : Vec Pattern n}
--     -> x1 ∷ xs1 ≡ x2 ∷ xs2 
--     -> xs1 ≡ xs2
-- cons-inj-tl refl = refl

K-inj-kn : ∀{n1 n2 k1 k2}
    -> {ps1 : Vec Pattern n1}
    -> {ps2 : Vec Pattern n2}
    -> K k1 n1 ps1 ≡ K k2 n2 ps2 
    -> k1 ≡ k2 × n1 ≡ n2
K-inj-kn refl = refl , refl

-- K-inj-ps : ∀{n k1 k2}
--     -> {ps1 ps2 : Vec Pattern n}
--     -> K k1 n ps1 ≡ K k2 n ps2 
--     -> ps1 ≡ ps2
-- K-inj-ps refl = refl

cons-preunify : ∀{s1 s2 p1 p2 k n} 
    -> {ps1 ps2 : Vec Pattern n}
    -> s1 , s2 preunifies p1 , p2
    -> s1 , s2 preunifies K k (suc n) (p1 ∷ ps1) , K k (suc n) (p2 ∷ ps2)
cons-preunify {s1} {s2} {p1} {p2} {k} {n} {ps1} {ps2} (PU pu) = PU pu'
    where 
    pu' : (s1' s2' : Sub)
      -> s1' , s2' unifies K k (suc n) (p1 ∷ ps1) , K k (suc n) (p2 ∷ ps2) 
      -> (s1' ⊑ s1) × (s2' ⊑ s2) 
    pu' s1' s2' (Unify u) = pu s1' s2' (Unify (cons-inj (K-inj-ps u)))

cons-preunify-tl :  ∀{s1 s2 p1 p2 k n} 
    -> {ps1 ps2 : Vec Pattern n}
    -> s1 , s2 preunifies K k n ps1 , K k n ps2
    -> s1 , s2 preunifies K k (suc n) (p1 ∷ ps1) , K k (suc n) (p2 ∷ ps2)
cons-preunify-tl {s1} {s2} {p1} {p2} {k} {n} {ps1} {ps2} (PU pu) = PU pu'
    where 
    pu' : (s1' s2' : Sub)
      -> s1' , s2' unifies K k (suc n) (p1 ∷ ps1) , K k (suc n) (p2 ∷ ps2) 
      -> (s1' ⊑ s1) × (s2' ⊑ s2) 
    pu' s1' s2' (Unify u) = pu s1' s2' (Unify (cong (K k n) (cons-inj-tl (K-inj-ps u))))

-- split-leq : (p1 p2 : Pattern) -> (s1 s2 : Sub) -> (pu : s1 , s2 preunifies p1 , p2) -> Set 
-- split-leq p1 p2 s1 s2 (PU pu) = (s1' s2' : Sub) -> (u : s1' , s2' unifies p1 , p2) -> helper s1' s2' u
--     where 
--     helper : (s1' s2' : Sub) -> (u : s1' , s2' unifies p1 , p2) -> Set 
--     helper s1' s2' u with pu s1' s2' u 
--     ... | Prec sp1 eq1 , Prec sp2 eq2 = metric sp1 sp2 (s1 [ p1 ]) (s2 [ p2 ]) < metric s1' s2' p1 p2

-- split-leq-cons-preunify : ∀{s1 s2 p1 p2 k n pu}
--     -> {ps1 ps2 : Vec Pattern n}
--     -> split-leq p1 p2 s1 s2 pu
--     -> split-leq (K k (suc n) (p1 ∷ ps1))
--       (K k (suc n) (p2 ∷ ps2)) s1 s2 (cons-preunify pu)
-- split-leq-cons-preunify {s1} {s2} {p1} {p2} {k} {n} {PU pu} {ps1} {ps2} leq s1' s2' (Unify u) with pu s1' s2' ((Unify (cons-inj (K-inj-ps u)))) in eq
-- ... | Prec sp1 eq1 , Prec sp2 eq2 = {!   !}

{-# REWRITE +-suc #-}

metric-lemma-1 : ∀{s3 s4 p1 p2 k1 n1}
    -> {ps1 ps2 : Vec Pattern n1}
    -> metric s3 s4 p1 p2 > 0
    -> metric s3 s4 (K k1 (suc n1) (p1 ∷ ps1)) (K k1 (suc n1) (p2 ∷ ps2)) > 0
metric-lemma-1 {s3} {s4} {p1} {p2} l with size-diff s3 p1 | size-diff s4 p2
... | zero | suc _ = s≤s z≤n
... | suc _ | _ = s≤s z≤n

metric-lemma-2 : ∀{s3 s4 p1 p2 k1 n1}
    -> {ps1 ps2 : Vec Pattern n1}
    -> metric s3 s4 (K k1 n1 ps1) (K k1 n1 ps2) > 0
    -> metric s3 s4 (K k1 (suc n1) (p1 ∷ ps1)) (K k1 (suc n1) (p2 ∷ ps2)) > 0
metric-lemma-2 {s3} {s4} {p1} {p2} {k1} {n1} {ps1} {ps2} l with size-diff s3 (K k1 n1 ps1) | size-diff s4 (K k1 n1 ps2)
... | zero | suc _ = s≤s z≤n
... | suc _ | _ = s≤s z≤n

-- begin machine generated code

-- Helper: (a + b) + (c + d) ≡ (a + c) + (b + d)
+-interchange : ∀ a b c d -> (a + b) + (c + d) ≡ (a + c) + (b + d)
+-interchange a b c d = begin
    (a + b) + (c + d)   ≡⟨ +-assoc a b (c + d) ⟩
    a + (b + (c + d))    ≡⟨ cong (a +_) (sym (+-assoc b c d)) ⟩
    a + ((b + c) + d)    ≡⟨ cong (λ x → a + (x + d)) (+-comm b c) ⟩
    a + ((c + b) + d)    ≡⟨ cong (a +_) (+-assoc c b d) ⟩
    a + (c + (b + d))    ≡⟨ sym (+-assoc a c (b + d)) ⟩
    (a + c) + (b + d)    ∎
    where open ≡-Reasoning

-- Lemma 1: size-diff s p + size p ≡ size (s [ p ])

mutual
    size-sub-vec : ∀{n}
        -> (s : Sub)
        -> (ps : Vec Pattern n)
        -> sum (map (size-diff s) ps) + sum (map size ps) ≡ sum (map size (map (_[_] s) ps))
    size-sub-vec s [] = refl
    size-sub-vec s (p ∷ ps)
        rewrite sym (size-sub s p)
        rewrite sym (size-sub-vec s ps)
        = +-interchange (size-diff s p) (sum (map (size-diff s) ps)) (size p) (sum (map size ps))

    size-sub : (s : Sub)
        -> (p : Pattern)
        -> size-diff s p + size p ≡ size (s [ p ])
    size-sub s (X x) = +-identityʳ (size (s x))
    size-sub s (K k n ps) rewrite sym (size-sub-vec s ps) = refl

-- Lemma 2: size-diff decomposes over composition
mutual
    size-diff-comp-vec : ∀{n}
        -> (sp s : Sub)
        -> (ps : Vec Pattern n)
        -> sum (map (size-diff (sp ∘ s)) ps) ≡ sum (map (size-diff sp) (map (_[_] s) ps)) + sum (map (size-diff s) ps)
    size-diff-comp-vec sp s [] = refl
    size-diff-comp-vec sp s (p ∷ ps)
        rewrite size-diff-comp sp s p
        rewrite size-diff-comp-vec sp s ps
        = sym (+-interchange (size-diff sp (s [ p ])) (sum (map (size-diff sp) (map (_[_] s) ps))) (size-diff s p) (sum (map (size-diff s) ps)))

    size-diff-comp : (sp s : Sub)
        -> (p : Pattern)
        -> size-diff (sp ∘ s) p ≡ size-diff sp (s [ p ]) + size-diff s p
    size-diff-comp sp s (X x) = sym (size-sub sp (s x))
    size-diff-comp sp s (K k n ps) = size-diff-comp-vec sp s ps

-- Lemma 3: metric decomposes over composition
metric-comp : (sp1 sp2 s1' s2' : Sub) (p1 p2 : Pattern)
    -> metric (sp1 ∘ s1') (sp2 ∘ s2') p1 p2
       ≡ metric sp1 sp2 (s1' [ p1 ]) (s2' [ p2 ]) + metric s1' s2' p1 p2
metric-comp sp1 sp2 s1' s2' p1 p2
    rewrite size-diff-comp sp1 s1' p1
    rewrite size-diff-comp sp2 s2' p2
    = +-interchange (size-diff sp1 (s1' [ p1 ])) (size-diff s1' p1) (size-diff sp2 (s2' [ p2 ])) (size-diff s2' p2)

-- Lemma 4: arithmetic helper
-- if a + b ≤ suc n and 1 ≤ b then a ≤ n
arith-helper : ∀{a b n} -> a + b ≤ suc n -> 1 ≤ b -> a ≤ n
arith-helper {zero} _ _ = z≤n
arith-helper {suc a} {suc b} {suc n} (s≤s h) (s≤s _) = s≤s (arith-helper {a} {suc b} {n} h (s≤s z≤n))

-- The final result
metric-inequality : ∀{s1 s2 s1' s2' sp1 sp2 p1 p2 n}
    -> metric s1 s2 p1 p2 ≤ suc n
    -> s1 ≡ sp1 ∘ s1'
    -> s2 ≡ sp2 ∘ s2'
    -> metric s1' s2' p1 p2 > 0
    -> metric sp1 sp2 (s1' [ p1 ]) (s2' [ p2 ]) ≤ n
metric-inequality {s1} {s2} {s1'} {s2'} {sp1} {sp2} {p1} {p2} {n} eq eq1 eq2 lt
    rewrite eq1 rewrite eq2
    rewrite metric-comp sp1 sp2 s1' s2' p1 p2
    = arith-helper {metric sp1 sp2 (s1' [ p1 ]) (s2' [ p2 ])} {metric s1' s2' p1 p2} {n} eq lt

-- end machine generated code

data SplitResult (p1 p2 : Pattern) : Set where
    SplitEC : equiv-constructor p1 p2
        -> SplitResult p1 p2
    SplitPU : (s1 s2 : Sub)
        -> (pu : s1 , s2 preunifies p1 , p2)
        -> metric s1 s2 p1 p2 > 0
        -> SplitResult p1 p2

{-# TERMINATING #-}
splitter : ∀{s1 s2 p1 p2}
        -> s1 , s2 unifies p1 , p2
        -> SplitResult p1 p2 
splitter {s1} {s2} {X _} {X _} u = SplitEC ECX

splitter {s1} {s2} {K k1 zero ps1} {K k2 (suc n2) ps2} (Unify ())
splitter {s1} {s2} {K k1 (suc n1) ps1} {K k2 zero ps2} (Unify ())
splitter {s1} {s2} {K k1 zero []} {K k2 zero []} (Unify refl) = SplitEC (ECK tt)
splitter {s1} {s2} {K k1 (suc n1) (p1 ∷ ps1)} {K k2 (suc n2) (p2 ∷ ps2)} (Unify u) with K-inj-kn u
splitter {s1} {s2} {K k1 (suc n1) (p1 ∷ ps1)} {K .(k1) (suc n2) (p2 ∷ ps2)} (Unify u) | refl , refl with splitter {s1} {s2} {p1} {p2} (Unify (cons-inj (K-inj-ps u))) 
... | SplitPU s3 s4 pu lt = SplitPU s3 s4 (cons-preunify pu) (metric-lemma-1 {s3} {s4} {p1} {p2} {k1} {n1} {ps1} {ps2} lt)
... | SplitEC ec with splitter {s1} {s2} {K k1 (n1) (ps1)} {K k1 (n2) (ps2)} (Unify (cong (K k1 n1) (cons-inj-tl (K-inj-ps u))))
... | SplitEC (ECK ecs) = SplitEC (ECK (ec , ecs))
... | SplitPU s3 s4 pu lt = SplitPU s3 s4 (cons-preunify-tl pu) (metric-lemma-2 {s3} {s4} {p1} {p2} {k1} {n1} {ps1} {ps2} lt)

splitter {s1} {s2} {X x} {K k n ps} u = SplitPU s1' sid (PU pu) ineq
    where
    s1' : Sub
    s1' x' with x ≟v x' 
    ... | yes refl = K k n (freshesL n)
    ... | no _ = X (R x')

    ineq : metric s1' sid (X x) (K k n ps) > 0
    ineq with x ≟v x
    ... | yes refl = s≤s z≤n
    ... | no neq = ⊥-elim (neq refl)

    pu : (s1'' s2'' : Sub)
        -> s1'' , s2'' unifies X x , K k n ps
        -> (s1'' ⊑ s1') × (s2'' ⊑ sid)
    pu s1'' s2'' (Unify eq) = (Prec (sp ps) (funext equiv)) , (Prec s2'' refl)
        where 
        sp : ∀{n} -> (ps : Vec Pattern n) -> Sub 
        sp ps' y with cleave y 
        ... | inj₁ x' = childfold s2'' ps' x'
        ... | inj₂ x' = s1'' x'

        equiv : (x' : Var) → s1'' x' ≡ ((sp ps) ∘ s1') x'
        equiv x' with x ≟v x' 
        equiv x' | no _ with cleave (R x') in eq'
        equiv x' | no _ | inj₁ _ = refl
        equiv x' | no _ | inj₂ _ with eq' 
        equiv x' | no _ | inj₂ _ | refl = refl
        equiv x' | yes refl = trans eq (cong (K k n) (equiv-children ps PrefixSelf))
            where
            equiv-child2 : ∀{n' n'' p' }
                -> {ps' : Vec Pattern n''}
                -> indexof ps' n' p'
                -> s2'' [ p' ] ≡ childfold s2'' ps' (Fresh n')
            equiv-child2 {n'} {n'' = n''} IndexOfHead with Fresh n' ≟v Fresh n'
            equiv-child2 {n'} IndexOfHead | yes refl  = refl
            equiv-child2 {n'} IndexOfHead | no neq = ⊥-elim (neq refl)
            equiv-child2 {n'} {n'' = n''} (IndexOfCons i) with Fresh n' ≟v Fresh n''
            equiv-child2 {n'} (IndexOfCons i) | yes eq' with Fresh-inj eq'
            equiv-child2 {n'} {n'' = suc n''} (IndexOfCons i) | yes _ | refl with Fresh (suc n'') ≟v Fresh n''
            equiv-child2 {n'} (IndexOfCons i) | yes _ | refl | yes eq' with Fresh-inj eq'
            equiv-child2 {n'} (IndexOfCons i) | yes _ | refl | yes _ | ()
            equiv-child2 {n'} (IndexOfCons i) | yes _ | refl | no _ = equiv-child2 i
            equiv-child2 {n'} {n'' = suc n''} (IndexOfCons i) | no neq with Fresh n' ≟v Fresh n''
            equiv-child2 {n'} (IndexOfCons i) | no neq | yes eq' rewrite eq' = ⊥-elim (lt-not-eq (index-lt-length i) (Fresh-inj eq'))
            equiv-child2 {n'} (IndexOfCons i) | no neq | no neq' = equiv-child2 i

            equiv-child1 : ∀{n' n'' p'}
                -> {ps' : Vec Pattern n''}
                -> indexof ps' n' p'
                -> s2'' [ p' ] ≡ sp ps' (L (Fresh n'))
            equiv-child1 {n'} i with cleave (L (Fresh n')) in eq''
            equiv-child1 {n'} i | inj₁ x = equiv-child2 i 
            equiv-child1 {n'} i | inj₂ _ with eq'' 
            equiv-child1 {n'} i | inj₂ _ | () 

            equiv-children : {n' : ℕ} -> (ps' : Vec Pattern n') -> prefix ps ps' -> map (_[_] s2'') ps' ≡ map (_[_] (sp ps)) (freshesL n')
            equiv-children [] pref = refl
            equiv-children  {suc n'} (p' ∷ ps') pref = cong₂ _∷_ (equiv-child1 (prefix-to-index pref IndexOfHead)) (equiv-children ps' (PrefixCons pref))
        

splitter {s1} {s2} {K k n ps} {X x} u = SplitPU sid s2' (PU pu) ineq
    where
    s2' : Sub
    s2' x' with x ≟v x' 
    ... | yes refl = K k n (freshesL n)
    ... | no _ = X (R x')

    ineq : metric sid s2' (K k n ps) (X x) > 0
    ineq with x ≟v x
    ... | yes refl = s≤s z≤n
    ... | no neq = ⊥-elim (neq refl)

    pu : (s1'' s2'' : Sub)
        -> s1'' , s2'' unifies K k n ps , X x 
        -> (s1'' ⊑ sid) × (s2'' ⊑ s2')
    pu s1'' s2'' (Unify eq) = (Prec s1'' refl) , (Prec (sp ps) (funext equiv))
        where 
        sp : ∀{n} -> (ps : Vec Pattern n) -> Sub 
        sp ps' y with cleave y 
        ... | inj₁ x' = childfold s1'' ps' x'
        ... | inj₂ x' = s2'' x'

        equiv : (x' : Var) → s2'' x' ≡ ((sp ps) ∘ s2') x'
        equiv x' with x ≟v x' 
        equiv x' | no _ with cleave (R x') in eq'
        equiv x' | no _ | inj₁ _ = refl
        equiv x' | no _ | inj₂ _ with eq' 
        equiv x' | no _ | inj₂ _ | refl = refl
        equiv x' | yes refl = trans (sym eq) (cong (K k n) (equiv-children ps PrefixSelf))
            where
            equiv-child2 : ∀{n' n'' p' }
                -> {ps' : Vec Pattern n''}
                -> indexof ps' n' p'
                -> s1'' [ p' ] ≡ childfold s1'' ps' (Fresh n')
            equiv-child2 {n'} {n'' = n''} IndexOfHead with Fresh n' ≟v Fresh n'
            equiv-child2 {n'} IndexOfHead | yes refl  = refl
            equiv-child2 {n'} IndexOfHead | no neq = ⊥-elim (neq refl)
            equiv-child2 {n'} {n'' = n''} (IndexOfCons i) with Fresh n' ≟v Fresh n''
            equiv-child2 {n'} (IndexOfCons i) | yes eq' with Fresh-inj eq'
            equiv-child2 {n'} {n'' = suc n''} (IndexOfCons i) | yes _ | refl with Fresh (suc n'') ≟v Fresh n''
            equiv-child2 {n'} (IndexOfCons i) | yes _ | refl | yes eq' with Fresh-inj eq'
            equiv-child2 {n'} (IndexOfCons i) | yes _ | refl | yes _ | ()
            equiv-child2 {n'} (IndexOfCons i) | yes _ | refl | no _ = equiv-child2 i
            equiv-child2 {n'} {n'' = suc n''} (IndexOfCons i) | no neq with Fresh n' ≟v Fresh n''
            equiv-child2 {n'} (IndexOfCons i) | no neq | yes eq' rewrite eq' = ⊥-elim (lt-not-eq (index-lt-length i) (Fresh-inj eq'))
            equiv-child2 {n'} (IndexOfCons i) | no neq | no neq' = equiv-child2 i

            equiv-child1 : ∀{n' n'' p'}
                -> {ps' : Vec Pattern n''}
                -> indexof ps' n' p'
                -> s1'' [ p' ] ≡ sp ps' (L (Fresh n'))
            equiv-child1 {n'} i with cleave (L (Fresh n')) in eq''
            equiv-child1 {n'} i | inj₁ x = equiv-child2 i 
            equiv-child1 {n'} i | inj₂ _ with eq'' 
            equiv-child1 {n'} i | inj₂ _ | () 

            equiv-children : {n' : ℕ} -> (ps' : Vec Pattern n') -> prefix ps ps' -> map (_[_] s1'') ps' ≡ map (_[_] (sp ps)) (freshesL n')
            equiv-children [] pref = refl
            equiv-children  {suc n'} (p' ∷ ps') pref = cong₂ _∷_ (equiv-child1 (prefix-to-index pref IndexOfHead)) (equiv-children ps' (PrefixCons pref))

    

generalization-sized : ∀{s1 s2 p1 p2}
    -> (n : ℕ)
    -> metric s1 s2 p1 p2 ≤ n
    -> s1 , s2 unifies p1 , p2
    -> ∃[ s1' ] ∃[ s2' ] s1' , s2' mgu p1 , p2
generalization-sized {s1} {s2} {p1} {p2} zero eq u with size-diff s1 p1 in eq1 | size-diff s2 p2 in eq2
generalization-sized {s1} {s2} {p1} {p2} zero eq u | zero | zero with size-diff-zero s1 p1 eq1 | size-diff-zero s2 p2 eq2
generalization-sized zero eq (Unify u) | zero | zero | ec1 | ec2 rewrite u = generalization-equiv-constructor (equiv-constructor-trans ec1 (equiv-constructor-sym ec2))
generalization-sized {s1} {s2} {p1} {p2} (suc n) eq u with splitter u 
generalization-sized {s1} {s2} {p1} {p2} (suc n) eq u | SplitEC ec = generalization-equiv-constructor ec
generalization-sized {s1} {s2} {p1} {p2} (suc n) eq (Unify u) | SplitPU s1' s2' (PU pu) lt with pu s1 s2 (Unify u) in pu-eq
generalization-sized {s1} {s2} {p1} {p2} (suc n) eq (Unify u) | SplitPU s1' s2' (PU pu) lt | Prec sp1 eq1 , Prec sp2 eq2 with generalization-sized {sp1} {sp2} {s1' [ p1 ]} {s2' [ p2 ]} n inequation (Unify equation)
    where 
    equation : (sp1 ∘ s1') [ p1 ] ≡ (sp2 ∘ s2') [ p2 ]
    equation rewrite eq1 rewrite eq2 = u

    inequation : metric sp1 sp2 (s1' [ p1 ]) (s2' [ p2 ]) ≤ n
    inequation = metric-inequality {s1} {s2} {s1'} {s2'} {sp1} {sp2} {p1} {p2} {n} eq eq1 eq2 lt  
generalization-sized {s1} {s2} {p1} {p2} (suc n) eq u | SplitPU s1' s2' (PU pu) lt | Prec sp1 eq1 , Prec sp2 eq2 | s1'' , s2'' , MGU (Unify u') mgu = (s1'' ∘ s1') , (s2'' ∘ s2') , MGU (Unify u') mgu'
    where 
    mgu' : (sl sr : Sub) →
      sl , sr unifies p1 , p2 →
      (sl ⊑ (s1'' ∘ s1')) × (sr ⊑ (s2'' ∘ s2'))
    mgu' sl sr (Unify u'') with pu sl sr (Unify u'')
    ... | Prec sp1' eq1' , Prec sp2' eq2' with mgu sp1' sp2' (Unify equation) 
        where 
        equation : (sp1' ∘ s1') [ p1 ] ≡ (sp2' ∘ s2') [ p2 ]
        equation rewrite eq1' rewrite eq2' = u''
    ... | Prec slp eql , Prec srp eqr = (Prec slp equation1) , Prec srp equation2
        where 
        equation1 : sl ≡ (slp ∘ s1'') ∘ s1'
        equation1 rewrite eql rewrite eq1' = refl
        equation2 : sr ≡ (srp ∘ s2'') ∘ s2'
        equation2 rewrite eqr rewrite eq2' = refl

generalization : ∀{p1 p2 s1 s2}
    -> s1 , s2 unifies p1 , p2
    -> ∃[ s1' ] ∃[ s2' ] s1' , s2' mgu p1 , p2
generalization = generalization-sized _ (≤-reflexive refl)

existence : ∀{t1 t2 t3 r1 r2}
    -> (t1 ↦[ r1 ] t2)
    -> (t2 ↦[ r2 ] t3)
    -> ∃[ r ] r1 ∘r r2 ≡ r
existence (Step t1 t2 p1 p2 f1 s1 refl refl) (Step .t2 t3 p3 p4 f2 s2 eq refl) with generalization (Unify {s1} {s2} {p2} {p3} eq)
... | s1' , s2' , mgu = ((s1' [ p1 ]) ↦ (s2' [ p4 ]) [ ∘r-functional p1 p2 p3 p4 f1 f2 s1' s2' mgu ]) , (Comp f1 f2 s1' s2' mgu)
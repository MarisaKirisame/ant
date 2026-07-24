{-# OPTIONS --rewriting #-}

open import Data.Product hiding (map)
open import Data.Vec hiding ([_])
open import Relation.Binary.PropositionalEquality hiding ([_])

open import core

data pf : Set₁ where 
    PF : (graph : Pattern -> Pattern -> Set)
        -> (functional : (p1 p2 p3 : Pattern) -> graph p1 p2 -> graph p1 p3 -> p2 ≡ p3)
        -> pf

_≡pf_ : pf -> pf -> Set
PF g1 f1 ≡pf PF g2 f2 = (p1 p2 : Pattern) -> (g1 p1 p2 -> g2 p1 p2) × (g2 p1 p2 -> g1 p1 p2)

data ∘graph (g1 g2 : Pattern -> Pattern -> Set) (p1 p2 : Pattern) : Set where
        G : (p : Pattern)
            -> g1 p1 p
            -> g2 p p2
            -> (∘graph g1 g2) p1 p2

≡pf-refl : ∀{f}
    -> f ≡pf f
≡pf-refl {PF _ _} p1 p2 = (λ z → z) , (λ z → z)

≡pf-sym : ∀{f1 f2}
    -> f1 ≡pf f2 
    -> f2 ≡pf f1
≡pf-sym {PF _ _} {PF _ _} e p1 p2 with e p1 p2 
... | h1 , h2 = h2 , h1

≡pf-trans : ∀{f1 f2 f3}
    -> f1 ≡pf f2 
    -> f2 ≡pf f3
    -> f1 ≡pf f3
≡pf-trans {PF _ _} {PF _ _} {PF _ _} e1 e2 p1 p2 = (λ z → e2 p1 p2 .proj₁ (e1 p1 p2 .proj₁ z)) , (λ z → e1 p1 p2 .proj₂ (e2 p1 p2 .proj₂ z))

_∘pf_ : pf -> pf -> pf 
PF g1 f1 ∘pf PF g2 f2 = PF (∘graph g1 g2) graph-functional
    where 
    graph-functional : (p1 p2 p3 : Pattern) -> (∘graph g1 g2) p1 p2 -> (∘graph g1 g2) p1 p3 -> p2 ≡ p3
    graph-functional p1 p2 p3 (G p12 g112 g122) (G p13 g113 g133) 
        rewrite f1 p1 p12 p13 g112 g113 
        rewrite f2 p13 p2 p3 g122 g133 = refl
    
∘pf-≡pf : {f1 f2 f1' f2' : pf}
    -> (f1 ≡pf f1')
    -> (f2 ≡pf f2')
    -> ((f1 ∘pf f2) ≡pf (f1' ∘pf f2'))
∘pf-≡pf {PF g1 f1} {PF g2 f2} {PF g1' f1'} {PF g2' f2'} eq1 eq2 p1 p2 = helper1 , helper2
    where 
    helper1 : ∘graph g1 g2 p1 p2 → ∘graph g1' g2' p1 p2
    helper1 (G p in1 in2) = G p (eq1 p1 p .proj₁ in1) (eq2 p p2 .proj₁ in2)
    helper2 : ∘graph g1' g2' p1 p2 → ∘graph g1 g2 p1 p2
    helper2 (G p in1 in2) = G p (eq1 p1 p .proj₂ in1) (eq2 p p2 .proj₂ in2)


data ⟦⟧graph (p1 p2 p3 p4 : Pattern) : Set where
    G : (s : Sub)
        -> s [ p1 ] ≡ p3
        -> s [ p2 ] ≡ p4
        -> (⟦⟧graph p1 p2) p3 p4

⟦_⟧ : (r : Rule) -> pf 
⟦ p1 ↦ p2 [ f ] ⟧ = PF (⟦⟧graph p1 p2) graph-functional
    where
    graph-functional : (p3 p4 p5 : Pattern) → (⟦⟧graph p1 p2) p3 p4 → (⟦⟧graph p1 p2) p3 p5 → p4 ≡ p5
    graph-functional p3 p4 p5 (G s1 eq1 eq2) (G s2 eq3 eq4) with f s1 s2 (trans eq1 (sym eq3))
    ... | eq5 = trans (sym eq2) (trans eq5 eq4)

⟦⟧-compatible : 
    {r1 r2 : Rule}
    -> r1 ≡r r2 
    -> ⟦ r1 ⟧ ≡pf ⟦ r2 ⟧
⟦⟧-compatible {p1 ↦ p2 [ f1 ]} {p3 ↦ p4 [ f2 ]} (REquiv s1 s2 eq1 eq2 eq3 eq4) p5 p6 = helper1 , helper2
    where 
    helper1 : ⟦⟧graph p1 p2 p5 p6 → ⟦⟧graph p3 p4 p5 p6
    helper1 (G s eq5 eq6) = G (s ∘ s2) subhelper1 subhelper2
        where 
        subhelper1 : (s ∘ s2) [ p3 ] ≡ p5
        subhelper1 rewrite eq3 rewrite eq5 = refl
        subhelper2 : (s ∘ s2) [ p4 ] ≡ p6
        subhelper2 rewrite eq4 rewrite eq6 = refl
    helper2 : ⟦⟧graph p3 p4 p5 p6 → ⟦⟧graph p1 p2 p5 p6
    helper2 (G s eq5 eq6) = G (s ∘ s1) subhelper1 subhelper2
        where 
        subhelper1 : (s ∘ s1) [ p1 ] ≡ p5
        subhelper1 rewrite eq1 rewrite eq5 = refl
        subhelper2 : (s ∘ s1) [ p2 ] ≡ p6
        subhelper2 rewrite eq2 rewrite eq6 = refl

⟦⟧-compatible-inv : 
    {r1 r2 : Rule}
    -> ⟦ r1 ⟧ ≡pf ⟦ r2 ⟧
    -> r1 ≡r r2 
⟦⟧-compatible-inv {p1 ↦ p2 [ f1 ]} {p3 ↦ p4 [ f2 ]} eq with eq p1 p2 | eq p3 p4
⟦⟧-compatible-inv {p1 ↦ p2 [ f1 ]} {p3 ↦ p4 [ f2 ]} eq | eq1 , eq2 | eq3 , eq4 with eq1 (G sid refl refl) | eq4 (G sid refl refl)
⟦⟧-compatible-inv {p1 ↦ p2 [ f1 ]} {p3 ↦ p4 [ f2 ]} eq | eq1 , eq2 | eq3 , eq4 | G s1 eq5 eq6 | G s2 eq7 eq8 = REquiv s2 s1 eq7 eq8 eq5 eq6

⟦∘⟧ : {r1 r2 r : Rule}
    -> r1 ∘r r2 ≡ r 
    -> (⟦ r1 ⟧ ∘pf ⟦ r2 ⟧) ≡pf ⟦ r ⟧
⟦∘⟧ {p1 ↦ p2 [ f1 ]} {p3 ↦ p4 [ f2 ]} (Comp .f1 .f2 s1 s2 (MGU (Unify u) mgu)) p5 p6 = helper1 , helper2
    where 

    helper1 : ∘graph (⟦⟧graph p1 p2) (⟦⟧graph p3 p4) p5 p6 → ⟦⟧graph (s1 [ p1 ]) (s2 [ p4 ]) p5 p6
    helper1 (G p (G s3 eq1 eq2) (G s4 eq3 eq4)) with mgu s3 s4 (Unify (trans eq2 (sym eq3))) 
    ... | Prec s5 eq5 , Prec s6 eq6 with f2 (s5 ∘ s2) s4 subhelper
        where 
        subhelper : (s5 ∘ s2) [ p3 ] ≡ s4 [ p3 ]
        subhelper rewrite sym u = trans (cong (λ s -> s [ p2 ]) (sym eq5)) (trans eq2 (sym eq3))
    ... | thing = G s5 subhelper (trans thing eq4)
        where 
        subhelper : (s5 ∘ s1) [ p1 ] ≡ p5
        subhelper = trans (cong (λ s -> s [ p1 ]) (sym eq5)) eq1

    helper2 : ⟦⟧graph (s1 [ p1 ]) (s2 [ p4 ]) p5 p6 → ∘graph (⟦⟧graph p1 p2) (⟦⟧graph p3 p4) p5 p6
    helper2 (G s3 eq1 eq2) = G (s3 [ s1 [ p2 ] ]) (G (s3 ∘ s1) eq1 refl) (G (s3 ∘ s2) (cong (λ p -> s3 [ p ]) (sym u)) eq2)

∘r-compatible : (r1 r1' r2 r2' r r' : Rule)
    -> (r1 ≡r r1')
    -> (r2 ≡r r2')
    -> r1 ∘r r2 ≡ r
    -> r1' ∘r r2' ≡ r'
    -> (r ≡r r')
∘r-compatible r1 r1' r2 r2' r r' eq1 eq2 c1 c2 with ⟦∘⟧ c1 | ⟦∘⟧ c2 
... | eq3 | eq4 = 
    ⟦⟧-compatible-inv
    (≡pf-trans (≡pf-sym eq3) 
    (≡pf-trans (∘pf-≡pf (⟦⟧-compatible eq1) (⟦⟧-compatible eq2)) eq4))

∘pf-assoc : (f1 f2 f3 : pf)
    -> (f1 ∘pf (f2 ∘pf f3)) ≡pf ((f1 ∘pf f2) ∘pf f3)
∘pf-assoc (PF g1 _) (PF g2 _) (PF g3 _) p1 p2 = helper1 , helper2
    where 
    helper1 : ∘graph g1 (∘graph g2 g3) p1 p2 → ∘graph (∘graph g1 g2) g3 p1 p2
    helper1 (G p3 eq1 (G p4 eq2 eq3)) = G p4 (G p3 eq1 eq2) eq3

    helper2 : ∘graph (∘graph g1 g2) g3 p1 p2 → ∘graph g1 (∘graph g2 g3) p1 p2
    helper2 (G p4 (G p3 eq1 eq2) eq3) = G p3 eq1 (G p4 eq2 eq3)

∘r-assoc : ∀{r1 r2 r3 r12 r23 r123 r123'}
    -> r1 ∘r r2 ≡ r12 
    -> r2 ∘r r3 ≡ r23 
    -> r1 ∘r r23 ≡ r123
    -> r12 ∘r r3 ≡ r123'
    -> r123 ≡r r123'
∘r-assoc c1 c2 c3 c4 with ⟦∘⟧ c1 | ⟦∘⟧ c2 | ⟦∘⟧ c3 | ⟦∘⟧ c4 
... | eq1 | eq2 | eq3 | eq4 = 
    ⟦⟧-compatible-inv 
    (≡pf-trans (≡pf-sym eq3) 
    (≡pf-trans (∘pf-≡pf ≡pf-refl (≡pf-sym eq2)) 
    (≡pf-trans (∘pf-assoc _ _ _) 
    (≡pf-trans (∘pf-≡pf eq1 ≡pf-refl) eq4))))
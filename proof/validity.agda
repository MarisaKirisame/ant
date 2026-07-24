{-# OPTIONS --rewriting #-}

open import Data.Nat
open import Data.Vec hiding ([_])
open import Data.Empty
open import Data.Product hiding (map)
open import Data.Sum hiding (map)
open import Relation.Binary.PropositionalEquality hiding ([_])

open import core

↦+-↦* : ∀{p1 p2 R}
    -> p1 ↦+[ R ] p2
    -> p1 ↦*[ R ] p2
↦+-↦* (Step in1 step) = Cons in1 step (Refl _ _)
↦+-↦* (Cons in1 step steps) = Cons in1 step (↦+-↦* steps)

↦*-trans : ∀{p1 p2 p3 R}
    -> p1 ↦*[ R ] p2
    -> p2 ↦*[ R ] p3
    -> p1 ↦*[ R ] p3
↦*-trans (Refl _ _) steps2 = steps2
↦*-trans (Cons in1 step steps1) steps2 = Cons in1 step (↦*-trans steps1 steps2)

validity-lemma : ∀{R p1 p2 r1 r2 r}
    -> R r1 
    -> R r2 
    -> r1 ∘r r2 ≡ r 
    -> p1 ↦[ r ] p2 
    -> p1 ↦+[ R ] p2
validity-lemma {R} in1 in2 (Comp {p1} {p2} {p3} {p4} f1 f2 s1 s2 (MGU (Unify eq) _)) (Step _ _ _ _ _ s refl refl) = Cons in1 step1 step2
    where 
    step1 : (s [ s1 [ p1 ] ]) ↦[ p1 ↦ p2 [ f1 ] ] (s [ s1 [ p2 ] ])
    step1 = (Step _ _ _ _ _ (s ∘ s1) (∘-eq s s1 p1) (∘-eq s s1 p2))

    step2 : (s [ s1 [ p2 ] ]) ↦+[ R ] (s [ s2 [ p4 ] ]) 
    step2 rewrite eq = (Step in2 (Step _ _ _ _ _ (s ∘ s2) (∘-eq s s2 p3) (∘-eq s s2 p4)))

validity : ∀{r1 r2 r R}
    -> R r1 
    -> R r2 
    -> r1 ∘r r2 ≡ r 
    -> R ≅R R ∪[ r ]
validity {r1} {r2} {r} {R} in1 in2 comp = Equiv equiv1 equiv2
    where
    equiv1-lemma1 : ∀{p1 p2}
        -> p1 ↦*[ R ] p2
        -> p1 ↦*[ R ∪[ r ] ] p2
    equiv1-lemma1 (Refl _ _) = Refl _ _
    equiv1-lemma1 (Cons in1 step steps) = Cons (inj₁ in1) step (equiv1-lemma1 steps)

    equiv1-lemma2 : ∀{p}
        -> p ↦̸[ R ]
        -> p ↦̸[ R ∪[ r ] ]
    equiv1-lemma2 stops p' r' (inj₁ in1) step = stops p' r' in1 step
    equiv1-lemma2 stops p' r' (inj₂ refl) step with validity-lemma in1 in2 comp step 
    ... | Step in' step' = stops _ _ in' step'
    ... | Cons in' step' _ = stops _ _ in' step'

    equiv1 : ∀{p1 p2}
        -> p1 =>[ R ] p2
        -> p1 =>[ R ∪[ r ] ] p2
    equiv1 (Eval steps stops) = Eval (equiv1-lemma1 steps) (equiv1-lemma2 stops)

    equiv2-lemma1 : ∀{p1 p2}
        -> p1 ↦*[ R ∪[ r ] ] p2
        -> p1 ↦*[ R ] p2
    equiv2-lemma1 (Refl _ _) = Refl _ _
    equiv2-lemma1 (Cons (inj₁ in1) step steps) = Cons in1 step (equiv2-lemma1 steps)
    equiv2-lemma1 (Cons (inj₂ refl) step steps) with validity-lemma in1 in2 comp step
    ... | steps' = ↦*-trans (↦+-↦* steps') (equiv2-lemma1 steps)

    equiv2-lemma2 : ∀{p}
        -> p ↦̸[ R ∪[ r ] ]
        -> p ↦̸[ R ]
    equiv2-lemma2 stops step1 r in1 step2 = stops step1 r (inj₁ in1) step2

    equiv2 : ∀{p1 p2}
        -> p1 =>[ R ∪[ r ] ] p2
        -> p1 =>[ R ] p2
    equiv2 (Eval steps stops) = Eval (equiv2-lemma1 steps) (equiv2-lemma2 stops)
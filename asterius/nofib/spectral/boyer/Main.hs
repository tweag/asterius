

module Main (main) where

import System.Environment
import Control.Monad (forM_)

data Term               = Var Id |
                          Fun Id [Term] [Lemma]

instance Eq Term where
        Var i1 == Var i2                = i1 == i2
        Fun f1 ts1 _ == Fun f2 ts2 _    = f1 == f2 && ts1 == ts2
        _ == _                          = False



type Lemma              = (Term, Term)



type Substitution       = [(Id, Term)]



data Id       = A | B | C | D | X | Y | Z | U | W |
                ADD1 | AND | APPEND | CONS | CONSP | DIFFERENCE |
                DIVIDES | EQUAL | EVEN | EXP | F | FALSE |
                FOUR | GCD | GREATEREQP | GREATERP | IF | IFF |
                IMPLIES | LENGTH | LESSEQP | LESSP | LISTP | MEMBER |
                NIL | NILP | NLISTP | NOT | ODD | ONE | OR | PLUS |
                QUOTIENT | REMAINDER | REVERSE | SUB1 | TIMES | TRUE |
                TWO | ZERO | ZEROP
                deriving Eq




one_way_unify :: Term -> Term -> (Bool, Substitution)
one_way_unify term1 term2 = one_way_unify1 term1 term2 []

one_way_unify1 :: Term -> Term -> Substitution -> (Bool, Substitution)
one_way_unify1 term1 term2@(Var vid2) subst
        = if found
          then (term1 == v2, subst)
          else (True, (vid2,term1):subst)
          where (found, v2) = find vid2 subst
{-
        = case find vid2 subst of { (found, v2) ->
          if found
          then (term1 == v2, subst)
          else (True, (vid2,term1):subst)
          }
-}
one_way_unify1 (Fun f1 as1 _) (Fun f2 as2 _) subst
        | f1 == f2
        = one_way_unify1_lst as1 as2 subst
one_way_unify1 _ _ _ = (False, error "unify")

one_way_unify1_lst [] [] subst = (True, subst)
one_way_unify1_lst (t1:ts1) (t2:ts2) subst
        = (hd_ok && tl_ok, subst'')
          where (hd_ok, subst')  = one_way_unify1 t1 t2 subst
                (tl_ok, subst'') = one_way_unify1_lst ts1 ts2 subst'
one_way_unify1_lst _ _ _ = (False, error "unify_lst")


find :: Id -> Substitution -> (Bool, Term)
find vid []                   = (False, error "find")
find vid1 ((vid2,val2):bs)    = if vid1 == vid2
                                  then (True, val2)
                                  else find vid1 bs


apply_subst :: Substitution -> Term -> Term
apply_subst subst term@(Var vid)
        = if found then value else term
          where (found, value) = find vid subst
apply_subst subst (Fun f args ls)
        = Fun f (map (apply_subst subst) args) ls



rewrite :: Term -> Term
rewrite term@(Var _)
        = term
rewrite term@(Fun f args lemmas)
        = rewrite_with_lemmas (Fun f (map rewrite args) lemmas) lemmas

rewrite_with_lemmas :: Term -> [Lemma] -> Term
rewrite_with_lemmas term []
        = term
rewrite_with_lemmas term ((lhs, rhs):ls)
        = if unified
          then rewrite (apply_subst subst rhs)
          else rewrite_with_lemmas term ls
          where (unified, subst) = one_way_unify term lhs



tautp :: Term -> Bool
tautp x = tautologyp (rewrite x) [] []

tautologyp :: Term -> [Term] -> [Term] -> Bool
tautologyp x true_lst false_lst
        | truep  x true_lst     = True  -- trivially or assumed true
        | falsep x false_lst    = False -- trivially or assumed false
tautologyp (Fun IF [cond, t, e] ls) true_lst false_lst
        | truep cond true_lst   = tautologyp t true_lst false_lst
        | falsep cond false_lst = tautologyp e true_lst false_lst
        | otherwise             =  tautologyp t (cond:true_lst) false_lst
                                   && tautologyp e true_lst (cond:false_lst)
tautologyp _ _ _                = False



truep :: Term -> [Term] -> Bool
truep (Fun TRUE _ _)   _ = True
truep x l                = x `elem` l

falsep :: Term -> [Term] -> Bool
falsep (Fun FALSE _ _) _ = True
falsep x l               = x `elem` l



main = forM_ [1..100] $ const $ do
  (n:_) <- getArgs
  print (test (read n))

test :: Int -> Bool
test n = all test0 xs
 where xs = take n (repeat (Var X))
       {-# NOINLINE xs #-}

test0 xxxx = tautp (apply_subst subst0 theorem)
 where
        subst0 = [
                  (X,   f (plus (plus a b) (plus c zero))),
                  (Y,   f (times (times a b) (plus c d))),
                  (Z,   f (reverse_ (append (append a b) nil))),
                  (U,   equal (plus a b) (difference x y)),
                  (W,   lessp (remainder a b) (member a (length_ b)))
                 ]
        theorem = implies (and_ (implies xxxx y)
                                (and_ (implies y z)
                                      (and_ (implies z u) (implies u w))))
                          (implies x w)
-- Variables

        a       = Var A
        b       = Var B
        c       = Var C
        d       = Var D
        u       = Var U
        w       = Var W
        x       = Var X
        y       = Var Y
        z       = Var Z

-- Constants

        false   = Fun FALSE [] []
        nil     = Fun NIL   [] []
        true    = Fun TRUE  [] []
        zero    = Fun ZERO  [] []

-- Functions with their associated lemmas

        add1 a  = Fun ADD1 [a] []
        and_ a b
                = Fun AND [a,b] [
                  (and_ x y,            if_ x (if_ y true false) false)]
        append a b
                = Fun APPEND [a,b] [
                  (append (append x y) z,       append x (append y z))]
        cons a b
                = Fun CONS [a,b] []
        consp a = Fun CONSP [a] [
                  (consp (cons x y),            true)]
        difference a b                  -- natural numbers
                = Fun DIFFERENCE [a,b] [
                  (difference x x,                      zero),
                  (difference (plus x y) x,             y),
                  (difference (plus y x) x,             y),
                  (difference (plus x y) (plus x z),    difference y z),
                  (difference (plus y (plus x z)) x,    plus y z),
                  (difference (add1 (plus y z)) z,      add1 y),
                  (difference (add1 (add1 x)) two,      x)]
        divides a b
                = Fun DIVIDES [a,b] [
                  (divides x y,                 zerop (remainder y x))]
        equal a b
                = Fun EQUAL [a,b] [
                  (equal (plus x y) zero,       and_ (zerop x) (zerop y)),
                  (equal (plus x y) (plus x z), equal y z),
                  (equal zero (difference x y), not_ (lessp y x)),
                  (equal x (difference x y),    or_ (equal x zero)
                                                    (zerop y)),
                  (equal (times x y) zero,      or_ (zerop x) (zerop y)),
                  (equal (append x y) (append x z), equal y z),
                  (equal y (times x y),         or_ (equal y zero)
                                                    (equal x one)),
                  (equal x (times x y),         or_ (equal x zero)
                                                    (equal y one)),
                  (equal (times x y) one,       and_ (equal x one)
                                                     (equal y one)),
                  (equal (difference x y)
                         (difference z y),      if_ (lessp x y)
                                                    (not_ (lessp y z))
                                                    (if_ (lessp z y)
                                                         (not_ (lessp y x))
                                                         (equal x z))),
                  (equal (lessp x y) z,         if_ (lessp x y)
                                                    (equal true z)
                                                    (equal false z))]
        even_ a = Fun EVEN [a] [
                  (even_ x,                     if_ (zerop x)
                                                    true
                                                    (odd_ (sub1 x)))]
        exp_ a b
                = Fun EXP [a,b] [
                  (exp_ x (plus y z),           times (exp_ x y) (exp_ x z)),
                  (exp_ x (times y z),          exp_ (exp_ x y) z)]
        f a     = Fun F [a] []
        four    = Fun FOUR [] [
                  (four,                        add1 (add1 two))]
        gcd_ a b
                = Fun GCD [a,b] [
                  (gcd_ x y,                     gcd_ y x),
                  (gcd_ (times x z) (times y z), times z (gcd_ x y))]
        greatereqp a b
                = Fun GREATEREQP [a,b] [
                  (greatereqp x y,              not_ (lessp x y))]
        greaterp a b
                = Fun GREATERP [a,b] [
                  (greaterp x y,                lessp y x)]
        if_ a b c
                = Fun IF [a,b,c] [
                  (if_ (if_ x y z) u w,         if_ x (if_ y u w) (if_ z u w))]
        iff a b = Fun IFF [a,b] [
                  (iff x y,                     and_ (implies x y)
                                                     (implies y x))]
        implies a b
                = Fun IMPLIES [a,b] [
                  (implies x y,                 if_ x (if_ y true false) true)]
        length_ a
                = Fun LENGTH [a] [
                  (length_ (reverse_ x),        length_ x),
                  (length_ (cons x (cons y (cons z (cons u w)))),
                                                plus four (length_
                                                w))]
        lesseqp a b
                = Fun LESSEQP [a,b] [
                  (lesseqp x y,                 not_ (lessp y x))]
        lessp a b
                = Fun LESSP [a,b] [
                  (lessp (remainder x y) y,     not_ (zerop y)),
                  (lessp (quotient x y) x,      and_ (not_ (zerop x))
                                                     (lessp one y)),
                  (lessp (plus x y) (plus x z), lessp y z),
                  (lessp (times x z) (times y z),
                                                and_ (not_ (zerop z))
                                                     (lessp x y)),
                  (lessp y (plus x y),          not_ (zerop x))]
        listp a = Fun LISTP [a] [
                  (listp x,                     or_ (nilp x) (consp x))]
        member a b
                = Fun MEMBER [a,b] [
                  (member x (append y z),       or_ (member x y) (member x z)),
                  (member x (reverse_ y),       member x y)]
        nilp a  = Fun NILP [a] [
                  (nilp x,                      equal x nil)]
        nlistp a
                = Fun NLISTP [a] [
                  (nlistp x,                    not_ (listp x))]
        not_ a  = Fun NOT [a] [
                  (not_ x,                      if_ x false true)]
        odd_ a  = Fun ODD [a] [
                  (odd_ x,                      even_ (sub1 x))]
        one     = Fun ONE [] [
                  (one,                         add1 zero)]
        or_ a b = Fun OR [a,b] [
                  (or_ x y,                     if_ x true (if_ y true false))]
        plus a b
                = Fun PLUS [a,b] [
                  (plus (plus x y) z,           plus x (plus y z)),
                  (plus (remainder x y)
                      (times y (quotient x y)), x),
                  (plus x (add1 y),             add1 (plus x y))]
        quotient a b
                = Fun QUOTIENT [a,b] [
                  (quotient (plus x (plus x y))
                            two,                plus x (quotient y two)),
                  (quotient (times y x) y,      if_ (zerop y) zero x)]
        remainder a b
                = Fun REMAINDER [a,b] [
                  (remainder x one,             zero),
                  (remainder x x,               zero),
                  (remainder (times x y) x,     zero),
                  (remainder (times x y) y,     zero)]

        reverse_ a
                = Fun REVERSE [a] [
                  (reverse_ (append x y),       append (reverse_ y)
                                                       (reverse_ x))]
        sub1 a  = Fun SUB1 [a] [
                  (sub1 (add1 x),               x)]
        times a b
                = Fun TIMES [a,b] [
                  (times x (plus y z),          plus (times x y) (times x z)),
                  (times (times x y) z,         times x (times y z)),
                  (times x (difference y z),    difference (times y x)
                                                           (times z x)),
                  (times x (add1 y),            plus x (times x y))]
        two     = Fun TWO [] [
                  (two,                         add1 one)]
        zerop a = Fun ZEROP [a] [
                  (zerop x,                     equal x zero)]


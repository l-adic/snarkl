{-# LANGUAGE RebindableSyntax #-}

module Snarkl.Example.Lam where

import Data.Typeable
import Snarkl.Errors
import Snarkl.Language.Syntax
import Snarkl.Language.SyntaxMonad
import Snarkl.Language.TExpr
import Prelude hiding
  ( fromRational,
    negate,
    return,
    (&&),
    (*),
    (+),
    (-),
    (/),
    (>>),
    (>>=),
  )

----------------------------------------------------------
-- Substitutions
-- \sigma ::= Shift n + (Term * \sigma)
----------------------------------------------------------

type TFSubst = 'TFSum ('TFConst 'TField) ('TFProd ('TFConst TTerm) 'TFId)

type TSubst = 'TMu TFSubst

subst_nil n =
  do
    n' <- inl n
    roll n' :: Comp TSubst

subst_cons t sigma =
  do
    p <- pair t sigma
    p' <- inr p
    roll p' :: Comp TSubst

case_subst sigma f_shift f_cons =
  do
    sigma' <- unroll (sigma :: TExp TSubst Rational)
    case_sum f_shift go sigma'
  where
    go p =
      do
        t <- fst_pair p
        sigma' <- snd_pair p
        f_cons t sigma'

----------------------------------------------------------
-- Terms
-- t ::= Field + t + (t * t)
----------------------------------------------------------

type TF = 'TFSum ('TFConst 'TField) ('TFSum 'TFId ('TFProd 'TFId 'TFId))

type TTerm = 'TMu TF

varN ::
  TExp 'TField Rational ->
  Comp TTerm
varN e =
  do
    v <- inl e
    roll v

varN' ::
  Int ->
  Comp TTerm
varN' i =
  do
    v <- inl (exp_of_int i)
    roll v

lam ::
  TExp TTerm Rational ->
  Comp TTerm
lam t =
  do
    t' <- inl t
    v <- inr t'
    roll v

app ::
  TExp TTerm Rational ->
  TExp TTerm Rational ->
  Comp TTerm
app t1 t2 =
  do
    t <- pair t1 t2
    t' <- inr t
    v <- inr t'
    roll v

case_term ::
  ( Typeable ty,
    Zippable ty
  ) =>
  TExp TTerm Rational ->
  (TExp 'TField Rational -> Comp ty) ->
  (TExp TTerm Rational -> Comp ty) ->
  (TExp TTerm Rational -> TExp TTerm Rational -> Comp ty) ->
  Comp ty
case_term t f_var f_lam f_app =
  do
    t' <- unroll t
    case_sum f_var (case_sum f_lam go) t'
  where
    go p =
      do
        e1 <- fst_pair p
        e2 <- fst_pair p
        f_app e1 e2

is_lam :: TExp TTerm Rational -> Comp 'TBool
is_lam t =
  case_term
    t
    (const $ return false)
    (const $ return true)
    (\_ _ -> return false)

shift ::
  TExp 'TField Rational ->
  TExp TTerm Rational ->
  Comp TTerm
shift n t = fix go t
  where
    go self t0 =
      case_term
        t0
        (\m -> varN (n + m))
        ( \t' ->
            do
              t'' <- self t'
              lam t''
        )
        ( \t1 t2 ->
            do
              t1' <- self t1
              t2' <- self t2
              app t1' t2'
        )

compose sigma1 sigma2 =
  do
    p <- pair sigma1 sigma2 :: Comp ('TProd TSubst TSubst)
    fix go p
  where
    go self p0 =
      let recur s1 s2 = pair s1 s2 >>= self
       in do
            s1 <- fst_pair p0
            s2 <- snd_pair p0
            case_subst
              s2
              -- Var(m)
              ( \m ->
                  if return (zeq m)
                    then return s1
                    else
                      case_subst
                        s1
                        -- Var(n)
                        (\n -> subst_nil $ n + m)
                        -- _ . s1'
                        (\_ s1' -> subst_nil (m - 1.0) >>= recur s1')
              )
              -- t' . s2'
              ( \t' s2' ->
                  do
                    t'' <- subst_term s1 t'
                    s2'' <- recur s1 s2'
                    subst_cons t'' s2''
              )

subst_term sigma t =
  do
    p <- pair sigma t :: Comp ('TProd TSubst TTerm)
    fix go p
  where
    go self p0 =
      let recur sigma0 t0 = pair sigma0 t0 >>= self
       in do
            sigma0 <- fst_pair p0
            t0 <- snd_pair p0
            case_term
              t0
              -- Var(n)
              ( \n ->
                  case_subst
                    sigma0
                    (\m -> varN $ n + m)
                    ( \t' sigma' ->
                        do
                          if return (zeq n)
                            then return t'
                            else varN (n - 1.0) >>= recur sigma'
                    )
              )
              -- Lam t1
              ( \t1 ->
                  do
                    var0 <- varN 0.0
                    sigma1 <- subst_nil 1.0
                    sigma2 <- compose sigma1 sigma
                    sigma' <- subst_cons var0 sigma2
                    t1' <- recur sigma' t1
                    lam t1'
              )
              -- App t1 t2
              ( \t1 t2 ->
                  do
                    self1 <- recur sigma t1
                    self2 <- recur sigma t2
                    app self1 self2
              )

beta ::
  TExp TTerm Rational ->
  TExp TTerm Rational ->
  Comp TTerm
beta t1 t2 =
  case_term
    t1
    -- Var(_)
    (\_ -> failWith $ ErrMsg "beta expects an abstraction")
    -- Lam t1'
    ( \t1' ->
        do
          id_subst <- subst_nil 0.0
          sigma <- subst_cons t2 id_subst
          subst_term sigma t1'
    )
    -- App _ _
    (\_ _ -> failWith $ ErrMsg "beta expects an abstraction")

step :: TExp TTerm Rational -> Comp TTerm
step t =
  case_term
    t
    (\_ -> return t)
    (\_ -> return t)
    (\t1 t2 -> beta t1 t2)

whnf :: TExp TTerm Rational -> Comp TTerm
whnf t = fix go t
  where
    go self t0 =
      do
        t' <- step t0
        case_term
          t'
          (\_ -> failWith $ ErrMsg "unbound variable")
          (\_ -> return t')
          (\_ _ -> self t')

-- \x y -> x
term_lam :: Comp TTerm
term_lam =
  do
    x <- varN' 1
    t <- lam x
    lam t

term_app :: Comp TTerm
term_app =
  do
    t <- term_lam
    app t t

-- (\x y -> x) (\x1 y1 -> x1)
-- ~~> (\y -> (\x1 y1 -> x1))
beta_test1 =
  do
    t <- term_app
    whnf t
    return 0.0

{-# LANGUAGE GADTs,TypeSynonymInstances,FlexibleInstances #-}

module Constraints where

import qualified Data.Map as Map

import Common
import Field
import Poly
import R1CS

----------------------------------------------------------------
--            Intermediate Constraint Language                --
----------------------------------------------------------------

data COp = CAdd | CSub | CMult | CDiv

instance Show COp where
  show CAdd  = "+"
  show CSub  = "-"
  show CMult = "*"
  show CDiv  = "-*"

invert_op :: COp -> COp
invert_op op = case op of
  CAdd -> CSub
  CSub -> CAdd
  CMult -> CDiv
  CDiv -> CMult

interp_op :: Field a => COp -> a -> a -> a
interp_op op = case op of
  CAdd -> add
  CSub -> \a1 a2 -> a1 `add` (neg a2)
  CMult -> mult
  CDiv -> \a1 a2 -> if a2 == zero then zero else a1 `mult` (inv a2)

data Constraint a where
  CVal   :: Field a => (Var,a)  -> Constraint a -- x = c
  CVar   :: (Var,Var)           -> Constraint a -- x = y
  CBinop :: COp -> (Var,Var,Var) -> Constraint a -- x `op` y = z

instance Show a => Show (Constraint a) where
  show (CVal (x,c)) = show x ++ "==" ++ show c
  show (CVar (x,y)) = show x ++ "==" ++ show y
  show (CBinop op (x,y,z))
    = show x ++ show op ++ show y ++ "==" ++ show z    

type Assgn a = Map.Map Var a

format_err :: Field a => [Constraint a] -> Assgn a -> a -> a -> String
format_err cs env c d
  = show c ++ " == " ++ show d
    ++ ": inconsistent assignment, in constraint context: "
    ++ show cs ++ ", in partial assignment context: " ++ show env

-- | Starting from an initial partial assignment [env], solve the
-- constraints [cs] and return the resulting complete assignment.
-- If the constraints are unsolvable from [env], report the first
-- constraint that is violated (under normal operation, this error
-- case should NOT occur).
solve_constraints :: Field a => [Constraint a] -- constraints to be solved
                  -> Assgn a -- initial assignment
                  -> Assgn a -- resulting assignment
solve_constraints cs env0 = g env0 cs
  where g env [] = env
        g env (CVal (x,c) : cs') = g (Map.insert x c env) cs'
        g env (c0@(CVar (x,y)) : cs')
          = case (Map.lookup x env,Map.lookup y env) of
              (Nothing,Nothing) -> g env (cs' ++ [c0])
              (Just c,Nothing)  -> g (Map.insert y c env) cs'
              (Nothing,Just d)  -> g (Map.insert x d env) cs'
              (Just c,Just d)   ->
                if c == d then g env cs' else error $ format_err cs env c d 
        g env (c0@(CBinop op (x,y,z)) : cs')
          = let f_op  = interp_op op
                fn_op = interp_op (invert_op op)  
            in case (Map.lookup x env,Map.lookup y env,Map.lookup z env) of
              (Just c,Just d,Nothing) ->
                g (Map.insert z (c `f_op` d) env) (cs' ++ [c0])
              (Just c,Nothing,Just e) ->
                case op of
                  CAdd  -> g (Map.insert y (e `fn_op` c) env) (cs' ++ [c0])
                  CSub  -> g (Map.insert y (c `f_op` e) env) (cs' ++ [c0])
                  CMult -> g (Map.insert y (e `fn_op` c) env) (cs' ++ [c0])
                  CDiv  -> g (Map.insert y (c `f_op` e) env) (cs' ++ [c0])
              (Nothing,Just d,Just e) ->
                g (Map.insert x (e `fn_op` d) env) (cs' ++ [c0])
              (Just c,Just d,Just e)  ->
                if e == c `f_op` d then g env cs'
                else error $ format_err cs env e (c `f_op` d) 
              (_,_,_) ->
                case op of
                  CAdd -> g env (cs' ++ [c0])
                  CSub -> g env (cs' ++ [c0])
                  CMult -> g env (cs' ++ [c0])
                  CDiv -> g env (cs' ++ [c0])
                  
r1c_of_c :: Field a => Int -> Constraint a -> R1C a
r1c_of_c nw c = case c of
  CVal (x,c') -> R1C (const_poly nw one,var_poly nw x,const_poly nw c')
  CVar (x,y)  -> R1C (const_poly nw one,var_poly nw x,var_poly nw y)
  -- NOTE: The encoding in the x == y case is not entirely
  -- field-agnostic (it relies on a 1+1 != 0, with the right
  -- semantics; satisfied, e.g., by ZmodN for N >= 2). We need a more
  -- general encoding.
  CBinop CAdd  (x,y,z) ->
    if x /= y then R1C (const_poly nw one,add_poly nw x y,var_poly nw z)
    else R1C (const_poly nw (add one one),var_poly nw x,var_poly nw z)
  CBinop CSub  (x,y,z) -> R1C (const_poly nw one,sub_poly nw x y,var_poly nw z)  
  CBinop CMult (x,y,z) -> R1C (var_poly nw x,var_poly nw y,var_poly nw z)
  CBinop CDiv (_,_,_) -> error "r1c_of_c: Div case not yet implemented"

r1cs_of_cs :: Field a => Int -> [Constraint a] -> R1CS a
r1cs_of_cs nw cs = R1CS $ map (r1c_of_c nw) cs

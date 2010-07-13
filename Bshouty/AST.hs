{-# LANGUAGE DeriveDataTypeable #-}
module Bshouty.AST where

import Control.Monad
import Data.Generics
import Data.List
import qualified Data.Map as M
import Data.Maybe

import Bshouty.Model

data Pred = BVar String
          | T
          | F
          | And Pred Pred
          | Or Pred Pred
          | Not Pred
            deriving (Eq, Show, Data, Typeable)
                     
mkTrue :: Pred
mkTrue = T

mkFalse :: Pred
mkFalse = F

mkBVar :: String -> Pred
mkBVar bv = BVar bv

mkAnd :: Pred -> Pred -> Pred
mkAnd pred1 pred2 = pred1 `And` pred2

mkOr :: Pred -> Pred -> Pred
mkOr pred1 pred2 = pred1 `Or` pred2

mkNot :: Pred -> Pred
mkNot pred = Not pred

mkEq :: Pred -> Pred -> Pred
mkEq a b = mkAnd (mkNot a `mkOr` b) (a `mkOr` mkNot b)

subst :: String -> Pred -> Pred -> Pred
subst bv pred = everywhere (mkT _subst)
  where
    _subst :: Pred -> Pred
    _subst (BVar bv') | bv == bv' = pred
    _subst pred' = pred'

negateBVar :: String -> Pred -> Pred
negateBVar bv = everywhere (mkT _negateBVar)
  where
    _negateBVar :: Pred -> Pred
    _negateBVar pred'@(BVar bv') | bv == bv' = mkNot pred'
    _negateBVar pred' = pred'

substBool :: String -> Bool -> Pred -> Pred
substBool bv b = subst bv (if b then mkTrue else mkFalse)

substModel :: Model -> Pred -> Pred
substModel ls pred = foldr (\(bv, b) pred -> substBool bv b pred) pred ls

findAllBVars :: Pred -> [String]
findAllBVars = nub . sort . everything (++) ([] `mkQ` findBVar)
  where
    findBVar :: Pred -> [String]
    findBVar (BVar bv) = [bv]
    findBVar _ = []
    
xor :: Pred -> Model -> Pred
xor pred a = foldr _xor pred bvars
  where 
    bvars = findAllBVars pred
    m = M.fromList a
    _xor bv pred =
      case M.lookup bv m of
        Just True -> negateBVar bv pred
        _ -> pred

mdnf :: [Model] -> Pred
mdnf = foldr1 mkOr . map mdnf'

mdnf' :: Model -> Pred
mdnf' = foldr1 mkAnd . map (mkBVar . fst) . filter snd

eval :: Pred -> Maybe Bool
eval T = Just True
eval F = Just False
eval (And pred1 pred2) = liftM2 (&&) (eval pred1) (eval pred2)
eval (Or pred1 pred2) = liftM2 (||) (eval pred1) (eval pred2)
eval (Not pred) = not `liftM` eval pred
eval _ = Nothing

eval' :: Pred -> Bool
eval' = fromJust . eval
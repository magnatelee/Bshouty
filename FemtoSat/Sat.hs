module FemtoSat.Sat where

import Data.List
import Data.Maybe

import Bshouty.AST
import Bshouty.Model (Model)

data Sat = SAT Model
         | UNSAT
           deriving (Eq, Show)

enumBools :: Int -> [[Bool]]
enumBools 1 = [[True], [False]]
enumBools n = map (True:) next ++ map (False:) next
  where next = enumBools (n - 1)

sat :: Pred -> Sat
sat pred =
  maybe UNSAT (SAT . (assignments !!)) (findIndex id evals)
    where 
      bvars = findAllBVars pred
      bvectors = enumBools (length bvars)
      assignments = map (zip bvars) bvectors
      evals = map (eval' . flip substModel pred) assignments
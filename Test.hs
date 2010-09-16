module Main where

import Bshouty.AST
import Bshouty.Learner
import Bshouty.Teacher
import FemtoSat.Sat

-- target = foldr1 mkOr [(mkNot . mkBVar) ("b" ++ show i) | i <- [1..2]]
-- target = mkOr 
--          (mkAnd ((mkNot . mkBVar) "A") (mkBVar "B")) $
--          (mkAnd (mkBVar "A") ((mkNot . mkBVar) "B"))
target = mkAnd 
         (mkOr (mkBVar "A") (mkBVar "B"))
         (mkOr ((mkNot . mkBVar) "A") ((mkNot . mkBVar) "B"))
          

isMember = eval' . flip substModel target
isEq pred = 
  case sat $ mkNot $ mkEq target pred of
    UNSAT -> YES
    SAT u -> NO u
    
teacher = Teacher { queryEq = isEq, queryMember = isMember }

main = do
  let answer = cdnf teacher
  putStrLn $ show answer
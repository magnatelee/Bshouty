module Bshouty.Learner where

import Bshouty.AST
import Bshouty.Teacher
import Bshouty.Model hiding (xor)
import qualified Bshouty.Model as MO (xor)

import System.IO.Unsafe

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

snd3 :: (a, b, c) -> b
snd3 (a, b, c) = b

trd3 :: (a, b, c) -> c
trd3 (a, b, c) = c

walk :: Teacher -> Model -> Model -> Model
walk teacher u a = _inner (u, []) (a, [])
  where
    _inner :: (Model, Model) -> (Model, Model) -> Model
    _inner ([], u) ([], a) = reverse u
    _inner ((ui@(uv, ub)):us, u) (ai:as, a) =
      if ub /= snd ai
      then 
        let u' = reverse u ++ ((uv, not ub):us) in
        if queryMember teacher u'
        then _inner (u', []) (reverse a ++ (ai:as), [])
        else  _inner (us, ui:u) (as, ai:a)
      else _inner (us, ui:u) (as, ai:a)

cdnf :: Teacher -> Pred
cdnf teacher =
  case queryEq teacher mkTrue of
    YES -> mkTrue
    NO u -> cdnf' u [(mkFalse, [], u)]
      where
        cdnf' :: Model -> [(Pred, [Model], Model)] -> Pred
        cdnf' u approx =
          let h = foldr1 mkAnd $ map fst3 approx in
           case queryEq teacher h of
             YES -> h
             NO u' ->
               let 
                 i = [((hi, si, ai), f) | 
                      (hi, si, ai) <- approx,
                      let f = eval' $ substModel u' hi]
               in
                if null $ filter (not . snd) i
                then cdnf' u ((mkFalse, [], u'):approx)
                else 
                  let approx' =
                        map (\(hi, si, ai) -> (xor (mdnf si) ai, si, ai)) $
                        map (\((hi, si, ai), f) ->
                              if f
                              then (hi, si, ai)
                              else 
                                let ui = walk teacher u' ai in 
                                (hi, (ui `MO.xor` ai):si, ai)) i
                  in cdnf' u approx'
  
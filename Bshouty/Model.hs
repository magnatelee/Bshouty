module Bshouty.Model where

type Model = [(String, Bool)]

xor :: Model -> Model -> Model
xor = zipWith (\(bv1, b1) (bv2, b2) -> (bv1, b1 /= b2))



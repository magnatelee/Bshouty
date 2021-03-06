module Bshouty.Teacher where

import Bshouty.AST (Pred)
import Bshouty.Model (Model)

data Answer = YES
            | NO Model
              deriving (Show)

data Teacher = Teacher {
  queryEq :: Pred -> Answer,
  queryMember :: Model -> Bool
  }
module TPDB.Data.Rule where

import TPDB.Data.Identifier
import Data.Typeable
import Data.Bifunctor ( Bifunctor(bimap) )

data Relation = Strict |  Weak | Equal deriving ( Eq, Ord, Typeable, Show )

data Rule a = Rule
  { lhs :: !a, rhs :: !a
  , relation :: !Relation
  , top :: !Bool
  -- TPDB (XTC) represents SRS as TRS,
  -- e.g.,  "ab -> ba" is "a(b(x)) -> b(a(x))",
  -- and when we convert back (as we need for CPF),
  -- need to use the original variable in the rule
  , original_variable :: !(Maybe Identifier)
  , conditions :: ![(a, a)]
  }
    deriving ( Eq, Ord, Typeable )

strict :: Rule a -> Bool
strict u = unconditional u && case relation u of Strict -> True ; _ -> False

weak :: Rule a -> Bool
weak u = unconditional u && case relation u of Weak -> True ; _ -> False

unconditional :: Rule a -> Bool
unconditional = null . conditions

conditional :: Rule a -> Bool
conditional = not . unconditional

equal :: Rule a -> Bool
equal u = unconditional u && case relation u of Equal -> True ; _ -> False

instance Functor Rule where
    fmap f u = u { lhs = f $ lhs u, rhs = f $ rhs u, conditions = map (bimap f f) $ conditions u }

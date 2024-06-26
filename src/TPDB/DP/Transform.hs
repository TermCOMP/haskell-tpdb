{-# language OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

module TPDB.DP.Transform
  ( dp, mark, Marked
  , pattern Marked, pattern Original, pattern Auxiliary
  , isOriginal, isMarked, mark_top
  , defined
  ) where

import TPDB.Data
import TPDB.Pretty

import qualified Data.Set as S
import Control.Monad ( guard, forM )

import Data.Hashable
import GHC.Generics

data Mark = Orig
   | Mark
   | Aux -- ^ wat is this?
  deriving (Eq, Ord, Show, Generic)
instance Hashable Mark

data Marked a = Marked_Imp { contents :: !a
                       , mark :: !Mark
                       }
    deriving ( Show, Eq, Ord, Generic )

pattern Marked a = Marked_Imp { mark = Mark, contents = a }
pattern Original a = Marked_Imp { mark = Orig, contents = a }
pattern Auxiliary a = Marked_Imp { mark = Aux, contents = a }

isOriginal m = mark m == Orig
isMarked   m = mark m == Mark

instance Hashable a => Hashable (Marked a)

instance Pretty a => Pretty ( Marked a) where
   pretty m = let p = pretty (contents m) in case mark m of
       Orig -> p
       Mark -> p <> "#"
       Aux -> p

mark_top :: TermC v a => Term v a -> Term v (Marked a)
mark_top  (Node f args) =
          Node (Marked f) $ map (tmap Original) args

defined s = S.fromList $ do
                u <- rules s
                let Node f args = lhs u
                -- will raise exception if lhs is variable
                return f

-- | compute the DP transformed system.

dp :: (Eq v, Ord s, TermC v s)
   => RS s (Term v s)
   -> RS (Marked s) (Term v (Marked s))
dp s =
   let os = map ( \ u -> if conditions u /= [] then error "DP transformation does not support conditional rules" else
                          Rule { relation = Weak
                               , lhs = tmap Original $ lhs u
                               , rhs = tmap Original $ rhs u
                               , top = False
                               , original_variable = original_variable u
                               , conditions = []
                               } )
           $ rules s
       def = defined s
       us = do
            u <- rules s
            let -- ssubs = S.fromList $ strict_subterms $ lhs u
                walk r = if  -- S.member r ssubs
                          isStrictSubtermOf r (lhs u)
                         then [] else case r of
                    -- will raise exception if rhs contains
                    -- a variable that is not in lhs
                    Node f args ->
                        ( if S.member f def then (r :) else id )
                        ( args >>= walk )
            r <- walk $ rhs u
            return $ Rule { relation = Strict
                          , lhs = mark_top $ lhs u
                          , rhs = mark_top r
                          , top = True
                          , original_variable = original_variable u
                          , conditions = []
                          }
   in RS { signature = map Marked ( S.toList def )
                     ++ map Original ( signature s )
         , rules = us ++ os
         , separate = separate s
         }

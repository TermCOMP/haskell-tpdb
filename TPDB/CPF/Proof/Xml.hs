{-# language TypeSynonymInstances, FlexibleContexts, FlexibleInstances, UndecidableInstances, OverlappingInstances, IncoherentInstances, PatternSignatures, DeriveDataTypeable #-}

-- | from internal representation to XML, and back

module TPDB.CPF.Proof.Xml where

import TPDB.CPF.Proof.Type
import qualified TPDB.Data as T

import qualified Text.XML.HaXml.Escape as E
import qualified Text.XML.HaXml.Pretty as P

import Text.XML.HaXml.Types (QName (..) )
import Text.XML.HaXml.XmlContent.Haskell hiding ( element, many )
import Text.XML.HaXml.Types ( EncodingDecl(..), emptyST, XMLDecl(..) )

import TPDB.Xml 
import TPDB.Data.Xml 

import Data.List ( nub )
import Data.Char ( toLower )
import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Time as T
import Control.Monad
import Data.Typeable
import Data.Ratio

tox :: CertificationProblem -> Document ()
tox p = 
    let xd = XMLDecl "1.0" ( Just $ EncodingDecl "UTF-8" ) Nothing 
        pro = Prolog ( Just xd ) [] Nothing []
        [ CElem e _ ] = toContents p
    in  Document pro emptyST e []

instance XmlContent CertificationProblem where
   parseContents = error "parseContents not implemented"

   toContents cp = rmkel "certificationProblem"
         [ mkel "input" $ toContents ( input cp )
         , mkel "cpfVersion" [ nospaceString $ cpfVersion cp ]
         , mkel "proof" $ toContents ( proof cp )
         , mkel "origin" $ toContents ( origin cp )
         ]

instance XmlContent Origin where
   parseContents = error "parseContents not implemented"

   toContents o = case o of
       ProofOrigin t -> rmkel "proofOrigin" $ toContents t

instance XmlContent Tool where
   parseContents = error "parseContents not implemented"

   toContents t = rmkel "tool" 
         [ mkel "name"    [ nospaceString $ name    t ]
         , mkel "version" [ nospaceString $ version t ]
         ]

instance XmlContent CertificationProblemInput where
   parseContents = error "parseContents not implemented"

   toContents i = case i of
      TrsInput {} -> rmkel "trsInput" $ toContents ( trsinput_trs i )

instance ( Typeable v, Typeable c, XmlContent v , XmlContent c  ) 
        => XmlContent ( T.TRS v c ) where
   parseContents = error "parseContents not implemented"

   toContents s = rmkel "trs" 
       $ rmkel "rules" $ concat $ map toContents $ T.rules s

instance ( Typeable t, XmlContent t  ) 
        => XmlContent ( T.Rule t) where
   parseContents = error "parseContents not implemented"

   toContents u = rmkel "rule" $ concat
      [ rmkel "lhs" ( toContents $ T.lhs u )
      , rmkel "rhs" ( toContents $ T.rhs u )
      ]

instance XmlContent Proof where
   parseContents = error "parseContents not implemented"

   toContents p = case p of
       TrsTerminationProof p -> toContents p

{-
instance ( Typeable i , XmlContent i ) => XmlContent ( Sharp i ) where
   toContents s = case s of
       Plain p -> toContents p
       Sharp q -> rmkel "sharp" $ toContents  q
       -}

instance XmlContent DPS where
   parseContents = error "parseContents not implemented"

   toContents ( DPS rules ) = rmkel "dps" 
        $ rmkel "rules" $ rules >>= toContents

instance XmlContent TrsTerminationProof where
   parseContents = error "parseContents not implemented"

   toContents p = rmkel "trsTerminationProof" $ case p of
      RIsEmpty -> rmkel "rIsEmpty" []
      DpTrans {} -> rmkel "dpTrans" $ concat
          [ toContents $ dptrans_dps p
          , rmkel "markedSymbols" [ nospaceString "true" ]
          , toContents $ dptrans_dpProof p
          ]
      StringReversal {} -> rmkel "stringReversal" $ concat
          [ toContents $ trs p
          , toContents $ trsTerminationProof p
          ]
      RuleRemoval {} -> rmkel "ruleRemoval" $ concat
          [ toContents $ rr_orderingConstraintProof p
          , toContents $ trs p
          , toContents $ trsTerminationProof p
          ]

instance XmlContent Model where
  parseContents = error "parseContents not implemented"

  toContents model = rmkel "model" $ case model of
    FiniteModel carrierSize interpret ->
      rmkel "finiteModel" $ concat
        [ rmkel "carrierSize" [ nospaceString $ show carrierSize ]
        , toContents interpret
        ]

instance XmlContent DpProof where
  parseContents = error "parseContents not implemented"

  toContents p = rmkel "dpProof" $ case p of
    PIsEmpty -> rmkel "pIsEmpty" []
    RedPairProc {} -> case rppUsableRules p of
      Nothing -> rmkel "redPairProc" $ concat
        [ toContents $ rppOrderingConstraintProof p
        , toContents $ rppDps p
        , toContents $ rppDpProof p
        ]
      Just (DPS ur) -> rmkel "redPairUrProc" $ concat
        [ toContents $ rppOrderingConstraintProof p
        , toContents $ rppDps p
        , toContents $ rppDpProof p
        , rmkel "usableRules" $ rmkel "rules" $ concatMap toContents ur
        ]
    DepGraphProc cs -> rmkel "depGraphProc" $ concat $ map toContents cs

instance XmlContent DepGraphComponent where
    toContents dgc = rmkel "component" $ concat
        [ rmkel "realScc" $ toContents $ dgcRealScc dgc
        , rmkel "dps" $ toContents $ dgcDps dgc
        , rmkel "dpProof" $ toContents $ dgcDpProof dgc
        ]

instance XmlContent OrderingConstraintProof where
  parseContents = error "parseContents not implemented"

  toContents (OCPRedPair rp) = rmkel "orderingConstraintProof" 
                             $ toContents rp
           
instance XmlContent RedPair where
  parseContents = error "parseContents not implemented"

  toContents rp = rmkel "redPair" $ case rp of
    RPInterpretation i -> toContents i
    RPPathOrder      o -> toContents o

instance XmlContent Interpretation where
   parseContents = error "parseContents not implemented"

   toContents i = rmkel "interpretation" $
        rmkel "type" ( toContents $ interpretation_type i )
     ++ concatMap toContents ( interprets i )
      
instance XmlContent Interpretation_Type where
   parseContents = error "parseContents not implemented"

   toContents t = rmkel "matrixInterpretation" $ concat 
      [ toContents ( domain t )
      , rmkel "dimension"       [ nospaceString $ show $ dimension t ]
      , rmkel "strictDimension" [ nospaceString $ show $ strictDimension t ]
      ]
     
instance XmlContent Domain where
   parseContents = error "parseContents not implemented"

   toContents d = rmkel "domain" $ case d of
       Naturals -> rmkel "naturals" []
       Rationals delta -> rmkel "rationals" 
         $ rmkel "delta" $ toContents delta
       Arctic d -> rmkel "arctic" $ toContents d
       Tropical d -> rmkel  "tropical" $ toContents d

instance XmlContent Rational where
    parseContents = error "parseContents not implemented"

    toContents r = rmkel "rational" 
        [ mkel "numerator"   [ nospaceString $ show $ numerator   r ]
        , mkel "denominator" [ nospaceString $ show $ denominator r ]
        ]

instance XmlContent Interpret  where
   parseContents = error "parseContents not implemented"

   toContents i = rmkel "interpret" $ concat
                    [ toContents $ symbol i
                    , rmkel "arity" [ nospaceString $ show $ arity i ]
                    , toContents $ value i
                    ]

instance XmlContent Value where
   parseContents = error "parseContents not implemented"

   toContents v = case v of
      Polynomial p -> toContents p

instance XmlContent Polynomial where
   parseContents = error "parseContents not implemented"

   toContents p = rmkel "polynomial" $ case p of
       Sum     ps -> rmkel "sum"     $ concat ( map toContents ps )
       Product ps -> rmkel "product" $ concat ( map toContents ps )
       Polynomial_Coefficient c -> rmkel "coefficient" $ toContents c
       Polynomial_Variable v -> rmkel "variable" [ nospaceString v ]

instance XmlContent ArithFunction where
  parseContents = error "parseContents not implemented"

  toContents af = rmkel "arithFunction" $ case af of
    AFNatural  n      -> rmkel "natural"  [ nospaceString $ show n ]
    AFVariable n      -> rmkel "variable" [ nospaceString $ show n ]
    AFSum     afs     -> rmkel "sum"      $ concatMap toContents afs
    AFProduct afs     -> rmkel "product"  $ concatMap toContents afs
    AFMin     afs     -> rmkel "min"      $ concatMap toContents afs
    AFMax     afs     -> rmkel "max"      $ concatMap toContents afs
    AFIfEqual a b t f -> rmkel "ifEqual"  $ concatMap toContents [a,b,t,f]

instance XmlContent Coefficient where
   parseContents = error "parseContents not implemented"

   toContents v = case v of
       Matrix vs -> rmkel "matrix" $ concat ( map toContents vs )
       Vector cs -> rmkel "vector" $ concat ( map toContents cs )
       Coefficient_Coefficient i -> 
          rmkel "coefficient" $ toContents i

instance XmlContent Exotic where
    parseContents = error "parseContents not implemented"

    toContents e = case e of
       Minus_Infinite -> rmkel "minusInfinity" []
       E_Integer i -> rmkel "integer" [ nospaceString $ show i ]
       Plus_Infinite -> rmkel "plusInfinity" []

instance XmlContent Symbol where
  parseContents = error "parseContents not implemented"

  toContents (SymName name) = rmkel "name"  $ toContents name
  toContents (SymSharp sym) = rmkel "sharp" $ toContents sym
  toContents (SymLabel sym label) = rmkel "labeledSymbol" 
                                  $ toContents sym ++ (toContents label)

instance XmlContent Label where
  parseContents = error "parseContents not implemented"

  toContents (LblNumber is) = 
    rmkel "numberLabel" $ map (\i -> mkel "number" [ nospaceString $ show i ]) is

  toContents (LblSymbol ss) = rmkel "symbolLabel" $ concatMap toContents ss

instance XmlContent PathOrder where
  parseContents = error "parseContents not implemented"

  toContents (PathOrder ps as) = rmkel "pathOrder" $ concat
    [ rmkel "statusPrecedence" $ concatMap toContents ps
    , if null as then []
      else rmkel "argumentFilter" $ concatMap toContents as
    ]

instance XmlContent PrecedenceEntry where
  parseContents = error "parseContents not implemented"

  toContents (PrecedenceEntry s a p) = rmkel "statusPrecedenceEntry" $ concat
    [ toContents s
    , rmkel "arity"      [ nospaceString $ show a ]
    , rmkel "precedence" [ nospaceString $ show p ]
    , rmkel "lex"        [ ]
    ]

instance XmlContent ArgumentFilterEntry where
  parseContents = error "parseContents not implemented"

  toContents (ArgumentFilterEntry s a f) = rmkel "argumentFilterEntry" $ concat
    [ toContents s
    , rmkel "arity"      [ nospaceString $ show a ]
    , case f of 
        Left i   -> rmkel "collapsing" [ nospaceString $ show i ]
        Right is -> rmkel "nonCollapsing" 
                  $ map (\i -> mkel "position" [ nospaceString $ show i ]) is
    ]

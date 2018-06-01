{-# language Arrows, NoMonomorphismRestriction, PatternSignatures, OverloadedStrings #-}

module TPDB.CPF.Proof.Read where

import TPDB.CPF.Proof.Type 
import TPDB.Data

{-
import Text.XML.HXT.Arrow.XmlArrow
import Text.XML.HXT.Arrow.XmlState ( runX )
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Text.XML.HXT.Arrow.XmlOptions ( a_validate )
import Text.XML.HXT.DOM.XmlKeywords (v_0)

import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowTree

import qualified TPDB.CPF.Proof.Write as W -- for testing
import qualified Text.XML.HXT.Arrow.XmlState as X 

-}

import qualified Text.XML as X
import Text.XML.Cursor
import qualified Data.Text.Lazy as T
import Control.Monad.Catch

{- | dangerous: 
not all constructor arguments will be set.
the function produces something like

      CertificationProblem { input = CertificationProblemInput 
                          , proof = TrsTerminationProof undefined
                          }  
-}

readCP :: T.Text -> Either SomeException [CertificationProblem]
readCP t = ( fromDoc . fromDocument ) <$> X.parseText X.def t


fromDoc :: Cursor -> [ CertificationProblem ]
fromDoc c = child c >>= element "certificationProblem" >>= child >>= \ c -> 
  ( CertificationProblem
     <$> (element "input" c >>= getInput)
     <*> (element "cpfVersion" c >>= content )
     <*> (element "proof" c >>= getProof)
     <*> (element "origin" c >>= return [ignoredOrigin] )
  )



getCP = getChild "certificationProblem" >>> proc x -> do
    inp <- getInput <<< getChild "input" -< x
    pro <- getProof <<< getChild "proof" -< x
    ver <- getText <<< gotoChild "cpfVersion" -< x
    returnA -< CertificationProblem 
        { input = inp, proof = pro, cpfVersion = ver, origin = ignoredOrigin }

getInput = getTerminationInput <+> getComplexityInput <+> getACTerminationInput

getTerminationInput = hasName "input" >>> proc x -> do
    trsI <- getTrsInput <<< getChild "trsInput" -< x    
    returnA -< TrsInput $ RS { rules = trsI, separate = False }

getACTerminationInput = hasName "input" >>> proc x -> do
    acrs <- getChild "acRewriteSystem" -< x
    trsI <- getTrsInput -< acrs
    as <- listA getSymbol <<< getChild "Asymbols" -< acrs
    cs <- listA getSymbol <<< getChild "Csymbols" -< acrs
    returnA -< ACRewriteSystem
      { trsinput_trs = RS { rules = trsI, separate = False }
      , asymbols = as
      , csymbols = cs
      }

getSymbol = proc x -> do
  s <- getText <<< gotoChild "name" -< x
  returnA -< mk 0 s


getComplexityInput = hasName "input" >>> proc x -> do
    y <- getChild "complexityInput" -< x
    trsI <- getTrsInput <<< getChild "trsInput" -< y
    cm <- getComplexityMeasure -< y
    cc <- getComplexityClass -< y
    returnA -< ComplexityInput
        { trsinput_trs = RS { rules = trsI, separate = False }
        , complexityMeasure = cm
        , complexityClass = cc
        }

getComplexityMeasure = 
        getDummy "derivationalComplexity" DerivationalComplexity
    <+> getDummy "runtimeComplexity" RuntimeComplexity

getComplexityClass = proc x -> do
    d <- getText <<< gotoChild "polynomial" -< x
    returnA -< ComplexityClassPolynomial { degree = read d }

getTrsInput = proc x -> do
    sys <- getTrs <<< getChild "trs" -< x
    rels <- listA ( getTrsWith Weak <<< getChild "relativeRules" ) -< x
    returnA -< sys ++ concat rels

getTrs = getTrsWith Strict

getTrsWith s = proc x -> do
    str <- getRules s <<< getChild "rules" -< x
    returnA -< str

getProof = getDummy "trsTerminationProof" ( TrsTerminationProof undefined )
       <+> getDummy "trsNonterminationProof" ( TrsNonterminationProof undefined )
       <+> getDummy "relativeTerminationProof" ( RelativeTerminationProof undefined )
       <+> getDummy "relativeNonterminationProof" ( RelativeNonterminationProof undefined )
       <+> getDummy "complexityProof" ( ComplexityProof undefined )
       <+> getDummy "acTerminationProof" ( ACTerminationProof undefined )

getDummy t c = proc x -> do 
    getChild t -< x
    returnA -< c 

getRules str = proc x -> do
    returnA <<< listA ( getRule str  <<< getChild "rule" ) -< x

getRule str = proc x -> do
    l <-  getTerm <<< isElem <<< gotoChild "lhs" -< x
    r <-  getTerm <<< isElem <<< gotoChild "rhs" -< x
    returnA -< Rule { lhs = l, relation = str, rhs = r, top = False }


getTerm = getVar <+> getFunApp

getVar = proc x -> do
    nm <- getText <<< getChildren <<< hasName "var" -< x
    returnA -< Var $ mk 0 nm

getFunApp = proc x -> do
    sub <- hasName "funapp" -< x
    nm <- getText <<< gotoChild "name" -< sub
    gs <- listA ( getTerm <<< gotoChild "arg" ) -< sub
    let c = mk (length gs) nm
    returnA -< Node c gs
          
gotoChild tag = proc x -> do
    returnA <<< getChildren <<< getChild tag -< x

getChild tag = proc x -> do
    returnA <<< hasName tag <<< isElem <<< getChildren -< x



